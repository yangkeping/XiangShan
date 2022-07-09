/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import freechips.rocketchip.util.SRAMAnnotation
import xiangshan._
import utils._
import xiangshan.backend.fu.{PMPChecker, PMPReqBundle}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.util.HasCSRConst


@chiselName
class TLB(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule with HasCSRConst with HasPerfEvents {
  val io = IO(new TlbIO(Width, q))

  require(q.superAssociative == "fa")
  if (q.sameCycle || q.missSameCycle) {
    require(q.normalAssociative == "fa")
  }

  val req = io.requestor.map(_.req)
  val resp = io.requestor.map(_.resp)
  val ptw = io.ptw
  val pmp = io.pmp
  val ptw_resp = if (q.sameCycle) RegNext(ptw.resp.bits) else ptw.resp.bits
  val ptw_resp_v = if (q.sameCycle) RegNext(ptw.resp.valid, init = false.B) else ptw.resp.valid

  val sfence = io.sfence
  val csr = io.csr
  val satp = csr.satp
  val priv = csr.priv
  val ifecth = if (q.fetchi) true.B else false.B
  val mode = if (q.useDmode) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = if (EnbaleTlbDebug) (satp.mode === 8.U)
  else (satp.mode === 8.U && (mode < ModeM))

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(new VaBundle))
  val vpn = reqAddr.map(_.vpn)
  val cmd = req.map(_.bits.cmd)
  val valid = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)

  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // Normal page && Super page
  val normalPage = TlbStorage(
    name = "normal",
    associative = q.normalAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = q.normalNSets,
    nWays = q.normalNWays,
    saveLevel = q.saveLevel,
    normalPage = true,
    superPage = false
  )
  val superPage = TlbStorage(
    name = "super",
    associative = q.superAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = q.superNSets,
    nWays = q.superNWays,
    saveLevel = q.saveLevel,
    normalPage = q.normalAsVictim,
    superPage = true,
  )


  for (i <- 0 until Width) {
    normalPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = vpn(i),
      asid = csr.satp.asid,
      i = i
    )
    superPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = vpn(i),
      asid = csr.satp.asid,
      i = i
    )
  }

  normalPage.victim.in <> superPage.victim.out
  normalPage.victim.out <> superPage.victim.in
  normalPage.sfence <> io.sfence
  superPage.sfence <> io.sfence
  normalPage.csr <> io.csr
  superPage.csr <> io.csr

  val refill_now = ptw_resp_v
  def TLBNormalRead(i: Int) = {
    val (n_hit_sameCycle, normal_hit, normal_ppn, normal_perm) = normalPage.r_resp_apply(i)
    val (s_hit_sameCycle, super_hit, super_ppn, super_perm) = superPage.r_resp_apply(i)
    // assert(!(normal_hit && super_hit && vmEnable && RegNext(req(i).valid, init = false.B)))

    val hit = normal_hit || super_hit
    val hit_sameCycle = n_hit_sameCycle || s_hit_sameCycle
    val ppn = Mux(super_hit, super_ppn, normal_ppn)
    val perm = Mux(super_hit, super_perm, normal_perm)

    val pf = perm.pf
    val af = perm.af
    val cmdReg = if (!q.sameCycle) RegNext(cmd(i)) else cmd(i)
    val validReg = if (!q.sameCycle) RegNext(valid(i)) else valid(i)
    val offReg = if (!q.sameCycle) RegNext(reqAddr(i).off) else reqAddr(i).off
    val sizeReg = if (!q.sameCycle) RegNext(req(i).bits.size) else req(i).bits.size

    /** *************** next cycle when two cycle is false******************* */
    val miss = !hit && vmEnable
    val fast_miss = !super_hit && vmEnable
    val miss_sameCycle = (!hit_sameCycle || refill_now) && vmEnable
    hit.suggestName(s"hit_${i}")
    miss.suggestName(s"miss_${i}")

    XSDebug(validReg, p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn)} perm:${perm}\n")

    val paddr = Cat(ppn, offReg)
    val vaddr = SignExt(req(i).bits.vaddr, PAddrBits)
    req(i).ready := resp(i).ready
    resp(i).valid := validReg
    resp(i).bits.paddr := Mux(vmEnable, paddr, if (!q.sameCycle) RegNext(vaddr) else vaddr)
    resp(i).bits.miss := { if (q.missSameCycle) miss_sameCycle else miss }
    resp(i).bits.fast_miss := fast_miss
    resp(i).bits.ptwBack := ptw.resp.fire()

    // for timing optimization, pmp check is divided into dynamic and static
    // dynamic: superpage (or full-connected reg entries) -> check pmp when translation done
    // static: 4K pages (or sram entries) -> check pmp with pre-checked results
    val pmp_paddr = Mux(vmEnable, Cat(super_ppn, offReg), if (!q.sameCycle) RegNext(vaddr) else vaddr)
    pmp(i).valid := resp(i).valid
    pmp(i).bits.addr := pmp_paddr
    pmp(i).bits.size := sizeReg
    pmp(i).bits.cmd := cmdReg

    val ldUpdate = !perm.a && TlbCmd.isRead(cmdReg) && !TlbCmd.isAmo(cmdReg) // update A/D through exception
    val stUpdate = (!perm.a || !perm.d) && (TlbCmd.isWrite(cmdReg) || TlbCmd.isAmo(cmdReg)) // update A/D through exception
    val instrUpdate = !perm.a && TlbCmd.isExec(cmdReg) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPermFail = !(modeCheck && (perm.r || priv.mxr && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && (TlbCmd.isRead(cmdReg) && !TlbCmd.isAmo(cmdReg))
    val stPf = (stPermFail || pf) && (TlbCmd.isWrite(cmdReg) || TlbCmd.isAmo(cmdReg))
    val instrPf = (instrPermFail || pf) && TlbCmd.isExec(cmdReg)
    val fault_valid = vmEnable
    resp(i).bits.excp.pf.ld := (ldPf || ldUpdate) && fault_valid && !af
    resp(i).bits.excp.pf.st := (stPf || stUpdate) && fault_valid && !af
    resp(i).bits.excp.pf.instr := (instrPf || instrUpdate) && fault_valid && !af
    // NOTE: pf need && with !af, page fault has higher priority than access fault
    // but ptw may also have access fault, then af happens, the translation is wrong.
    // In this case, pf has lower priority than af

    val spm = normal_perm.pm // static physical memory protection or attribute
    val spm_v = !super_hit && vmEnable && q.partialStaticPMP.B // static pm valid; do not use normal_hit, it's too long.
    // for tlb without sram, tlb will miss, pm should be ignored outsize
    resp(i).bits.excp.af.ld    := (af || (spm_v && !spm.r)) && TlbCmd.isRead(cmdReg) && fault_valid
    resp(i).bits.excp.af.st    := (af || (spm_v && !spm.w)) && TlbCmd.isWrite(cmdReg) && fault_valid
    resp(i).bits.excp.af.instr := (af || (spm_v && !spm.x)) && TlbCmd.isExec(cmdReg) && fault_valid
    resp(i).bits.static_pm.valid := spm_v && fault_valid // ls/st unit should use this mmio, not the result from pmp
    resp(i).bits.static_pm.bits := !spm.c

    (hit, miss, validReg)
  }

  val readResult = (0 until Width).map(TLBNormalRead(_))
  val hitVec = readResult.map(_._1)
  val missVec = readResult.map(_._2)
  val validRegVec = readResult.map(_._3)

  // replacement
  def get_access(one_hot: UInt, valid: Bool): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR && valid
    res.bits := OHToUInt(one_hot)
    res
  }

  val normal_refill_idx = if (q.outReplace) {
    io.replace.normalPage.access <> normalPage.access
    io.replace.normalPage.chosen_set := get_set_idx(ptw_resp.entry.tag, q.normalNSets)
    io.replace.normalPage.refillIdx
  } else if (q.normalAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
    re.access(normalPage.access.map(_.touch_ways)) // normalhitVecVec.zipWithIndex.map{ case (hv, i) => get_access(hv, validRegVec(i))})
    re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNSets, q.normalNWays)
    re.access(normalPage.access.map(_.sets), normalPage.access.map(_.touch_ways))
    re.way(get_set_idx(ptw_resp.entry.tag, q.normalNSets))
  }

  val super_refill_idx = if (q.outReplace) {
    io.replace.superPage.access <> superPage.access
    io.replace.superPage.chosen_set := DontCare
    io.replace.superPage.refillIdx
  } else {
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNWays)
    re.access(superPage.access.map(_.touch_ways))
    re.way
  }

  val refill = ptw_resp_v && !sfence.valid && !satp.changed
  normalPage.w_apply(
    valid = { if (q.normalAsVictim) false.B
    else refill && ptw_resp.entry.level.get === 2.U },
    wayIdx = normal_refill_idx,
    data = ptw_resp,
    data_replenish = io.ptw_replenish
  )
  superPage.w_apply(
    valid = { if (q.normalAsVictim) refill
    else refill && ptw_resp.entry.level.get =/= 2.U },
    wayIdx = super_refill_idx,
    data = ptw_resp,
    data_replenish = io.ptw_replenish
  )

  // if sameCycle, just req.valid
  // if !sameCycle, add one more RegNext based on !sameCycle's RegNext
  // because sram is too slow and dtlb is too distant from dtlbRepeater
  for (i <- 0 until Width) {
    io.ptw.req(i).valid :=  need_RegNextInit(!q.sameCycle, validRegVec(i) && missVec(i), false.B) &&
      !RegNext(refill, init = false.B) &&
      param_choose(!q.sameCycle, !RegNext(RegNext(refill, init = false.B), init = false.B), true.B)
    io.ptw.req(i).bits.vpn := need_RegNext(!q.sameCycle, need_RegNext(!q.sameCycle, reqAddr(i).vpn))
  }
  io.ptw.resp.ready := true.B

  def need_RegNext[T <: Data](need: Boolean, data: T): T = {
    if (need) RegNext(data)
    else data
  }
  def need_RegNextInit[T <: Data](need: Boolean, data: T, init_value: T): T = {
    if (need) RegNext(data, init = init_value)
    else data
  }

  def param_choose[T <: Data](need: Boolean, truedata: T, falsedata: T): T = {
    if (need) truedata
    else falsedata
  }

  if (!q.shouldBlock) {
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_access" + Integer.toString(i, 10), validRegVec(i) && vmEnable && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("access" + Integer.toString(i, 10), validRegVec(i) && vmEnable)
    }
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_miss" + Integer.toString(i, 10), validRegVec(i) && vmEnable && missVec(i) && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("miss" + Integer.toString(i, 10), validRegVec(i) && vmEnable && missVec(i))
    }
  } else {
    // NOTE: ITLB is blocked, so every resp will be valid only when hit
    // every req will be ready only when hit
    for (i <- 0 until Width) {
      XSPerfAccumulate(s"access${i}", io.requestor(i).req.fire() && vmEnable)
      XSPerfAccumulate(s"miss${i}", ptw.req(i).fire())
    }

  }
  //val reqCycleCnt = Reg(UInt(16.W))
  //reqCycleCnt := reqCycleCnt + BoolStopWatch(ptw.req(0).fire(), ptw.resp.fire || sfence.valid)
  //XSPerfAccumulate("ptw_req_count", ptw.req.fire())
  //XSPerfAccumulate("ptw_req_cycle", Mux(ptw.resp.fire(), reqCycleCnt, 0.U))
  XSPerfAccumulate("ptw_resp_count", ptw.resp.fire())
  XSPerfAccumulate("ptw_resp_pf_count", ptw.resp.fire() && ptw.resp.bits.pf)

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(sfence.valid, p"Sfence: ${sfence}\n")
  XSDebug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire(), p"PTW req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  println(s"${q.name}: normal page: ${q.normalNWays} ${q.normalAssociative} ${q.normalReplacer.get} super page: ${q.superNWays} ${q.superAssociative} ${q.superReplacer.get}")

//   // NOTE: just for simple tlb debug, comment it after tlb's debug
  // assert(!io.ptw.resp.valid || io.ptw.resp.bits.entry.tag === io.ptw.resp.bits.entry.ppn, "Simple tlb debug requires vpn === ppn")

  val perfEvents = if(!q.shouldBlock) {
    Seq(
      ("access", PopCount((0 until Width).map(i => vmEnable && validRegVec(i)))              ),
      ("miss  ", PopCount((0 until Width).map(i => vmEnable && validRegVec(i) && missVec(i)))),
    )
  } else {
    Seq(
      ("access", PopCount((0 until Width).map(i => io.requestor(i).req.fire()))),
      ("miss  ", PopCount((0 until Width).map(i => ptw.req(i).fire()))         ),
    )
  }
  generatePerfEvent()
}

class TlbReplace(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbReplaceIO(Width, q))

  if (q.normalAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
    re.access(io.normalPage.access.map(_.touch_ways))
    io.normalPage.refillIdx := re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNSets, q.normalNWays)
    re.access(io.normalPage.access.map(_.sets), io.normalPage.access.map(_.touch_ways))
    io.normalPage.refillIdx := { if (q.normalNWays == 1) 0.U else re.way(io.normalPage.chosen_set) }
  }

  if (q.superAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNWays)
    re.access(io.superPage.access.map(_.touch_ways))
    io.superPage.refillIdx := re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNSets, q.superNWays)
    re.access(io.superPage.access.map(_.sets), io.superPage.access.map(_.touch_ways))
    io.superPage.refillIdx := { if (q.superNWays == 1) 0.U else re.way(io.superPage.chosen_set) }
  }
}

object TLB {
  def apply
  (
    in: Seq[BlockTlbRequestIO],
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    width: Int,
    shouldBlock: Boolean,
    q: TLBParameters
  )(implicit p: Parameters) = {
    require(in.length == width)

    val tlb = Module(new TLB(width, q))

    tlb.io.sfence <> sfence
    tlb.io.csr <> csr
    tlb.suggestName(s"tlb_${q.name}")

    if (!shouldBlock) { // dtlb
      for (i <- 0 until width) {
        tlb.io.requestor(i) <> in(i)
        // tlb.io.requestor(i).req.valid := in(i).req.valid
        // tlb.io.requestor(i).req.bits := in(i).req.bits
        // in(i).req.ready := tlb.io.requestor(i).req.ready

        // in(i).resp.valid := tlb.io.requestor(i).resp.valid
        // in(i).resp.bits := tlb.io.requestor(i).resp.bits
        // tlb.io.requestor(i).resp.ready := in(i).resp.ready
      }
    } else { // itlb
      //require(width == 1)
      (0 until width).map{ i =>
        tlb.io.requestor(i).req.valid := in(i).req.valid
        tlb.io.requestor(i).req.bits := in(i).req.bits
        in(i).req.ready := !tlb.io.requestor(i).resp.bits.miss && in(i).resp.ready && tlb.io.requestor(i).req.ready

        require(q.missSameCycle || q.sameCycle)
        // NOTE: the resp.valid seems to be useless, it must be true when need
        //       But don't know what happens when true but not need, so keep it correct value, not just true.B
        if (q.missSameCycle && !q.sameCycle) {
          in(i).resp.valid := tlb.io.requestor(i).resp.valid && !RegNext(tlb.io.requestor(i).resp.bits.miss)
        } else {
          in(i).resp.valid := tlb.io.requestor(i).resp.valid && !tlb.io.requestor(i).resp.bits.miss
        }
        in(i).resp.bits := tlb.io.requestor(i).resp.bits
        tlb.io.requestor(i).resp.ready := in(i).resp.ready
      }
      tlb.io.ptw_replenish <> DontCare // itlb only use reg, so no static pmp/pma
    }
    tlb.io.ptw
  }
}
