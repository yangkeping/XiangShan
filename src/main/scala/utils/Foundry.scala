/***************************************************************************************
 * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package utils

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable.ListBuffer

class FoundrySRAMExtraIO extends Bundle {
  val sram_type: String = "none"

  def getElements(direction: ActualDirection): Seq[(String, Data)] = {
    elements.filter(elem => DataMirror.directionOf(elem._2) == direction).toSeq.reverse
  }
  def getElementsData(direction: ActualDirection): Seq[Data] = getElements(direction).map(_._2)
  def asUInt(direction: ActualDirection): UInt = WireInit(MixedVecInit(getElementsData(direction))).asUInt
  def asUIntWidth(direction: ActualDirection): Int = getElementsData(direction).map(_.getWidth).sum
  def assignFromUInt(in: UInt, direction: ActualDirection): Unit = {
    val elem_data = getElementsData(direction)
    val elem_in = in.asTypeOf(MixedVecInit(elem_data).cloneType)
    elem_in.zip(elem_data).foreach{ case (in, d) => d := in }
  }
  def inputAsUIntWidth: Int = asUIntWidth(ActualDirection.Input)
  def inputFromUInt(in: UInt): Unit = assignFromUInt(in, ActualDirection.Input)
  def inputAsUInt: UInt = asUInt(ActualDirection.Input)
  def outputAsUIntWidth: Int = asUIntWidth(ActualDirection.Output)
  def outputFromUInt(in: UInt): Unit = assignFromUInt(in, ActualDirection.Output)
  def outputAsUInt: UInt = asUInt(ActualDirection.Output)
}

// Foundry-dependent IOs for RFs
class RFExtraIO extends FoundrySRAMExtraIO {
  override val sram_type: String = "rf"

  val trim_fuse_in = Input(UInt(11.W))
  val pwr_mgmt_in = Input(UInt(5.W))
  val sleep_fuse_in = Input(UInt(2.W))
  val pwr_mgmt_out = Output(Bool())
  val ip_reset_b = Input(Bool())
  val wrapper = new Bundle() {
    val rd_clk_en = Input(Bool())
    val wr_clk_en = Input(Bool())
  }
  val fscan = new Bundle() {
    val clkungate = Input(Bool())
    val ram = new Bundle() {
      val bypsel = Input(Bool())
      val wdis_b = Input(Bool())
      val rdis_b = Input(Bool())
      val init_en = Input(Bool())
      val init_val = Input(Bool())
    }
  }
  val output_reset = Input(Bool())
}

// Foundry-dependent IOs for SRAMs
class SRAMExtraIO extends FoundrySRAMExtraIO {
  override val sram_type: String = "sram"

  val trim_fuse_in = Input(UInt(20.W))
  val pwr_mgmt_in = Input(UInt(6.W))
  val sleep_fuse_in = Input(UInt(2.W))
  val pwr_mgmt_out = Output(Bool())
  val ip_reset_b = Input(Bool())
  val wrapper = new Bundle() {
    val clk_en = Input(Bool())
  }
  val fscan = new Bundle() {
    val clkungate = Input(Bool())
    val ram = new Bundle() {
      val bypsel = Input(Bool())
      val wdis_b = Input(Bool())
      val rdis_b = Input(Bool())
      val init_en = Input(Bool())
      val init_val = Input(Bool())
    }
  }
  val output_reset = Input(Bool())
}

object FoundrySRAMExtraIO {
  def apply(singlePort: Boolean): Option[FoundrySRAMExtraIO] = {
    if (singlePort) Some(new SRAMExtraIO) else Some(new RFExtraIO)
  }

  private var sram_id: Int = 0
  private val extra_instances = ListBuffer.empty[(Int, FoundrySRAMExtraIO)]
  def extra_name_in(id: Int): String = s"sram_foundry_extra_in_$id"
  def extra_name_out(id: Int): String = s"sram_foundry_extra_out_$id"
  def addExtraInstance(extra: FoundrySRAMExtraIO): Int = {
    val this_id = sram_id
    // Inputs
    val data_in_u = WireInit(0.U.asTypeOf(UInt(extra.inputAsUIntWidth.W)))
    BoringUtils.addSink(data_in_u, extra_name_in(this_id))
    extra.inputFromUInt(data_in_u)
    // Outputs
    val data_out_u = extra.outputAsUInt
    BoringUtils.addSource(data_out_u, extra_name_out(this_id))
    val instance = (this_id, extra)
    extra_instances += instance
    sram_id += 1
    this_id
  }
  def addConnector(): FoundrySRAMExtraConnector = {
    val sram_connector = Module(new FoundrySRAMExtraConnector(extra_instances))
    sram_connector.connect()
    sram_id = 0
    extra_instances.clear()
    sram_connector
  }
}

class FoundrySRAMExtraConnector(instances: Seq[(Int, FoundrySRAMExtraIO)]) extends Module {
  val sramTypes = instances.map(_._2.sram_type).distinct
  val sram = sramTypes.map(t => instances.filter(_._2.sram_type == t))

  // this is from dft controller
  val io = IO(new Bundle() {
    val dummy = Input(Bool())
  })
  // this is from srams
  val extra = sram.map(s => IO(Flipped(Vec(s.length, s.head._2.cloneType))))

  for (ex <- extra) {
    for (elem <- ex) {
      for ((_, data) <- elem.elements.toSeq.reverse) {
        if (DataMirror.directionOf(data) == ActualDirection.Output) {
          data := LFSR64().asTypeOf(data.cloneType)
        }
      }
    }
  }

  dontTouch(io)
  extra.foreach(ex => dontTouch(ex))

  def connect(): Unit = {
    io := DontCare
    extra.foreach(x => x := DontCare)
    for ((_extra, _sram) <- extra.zip(sram)) {
      for ((conn_extra, sram_index) <- _extra.zip(_sram.map(_._1))) {
        // Inputs (for SRAMs); Outputs (for Connector)
        val data_in_u = conn_extra.outputAsUInt
        BoringUtils.addSource(data_in_u, FoundrySRAMExtraIO.extra_name_in(sram_index))
        // Outputs (for SRAMs); Inputs (for Connector)
        val data_out_u = WireInit(0.U.asTypeOf(UInt(conn_extra.inputAsUIntWidth.W)))
        BoringUtils.addSink(data_out_u, FoundrySRAMExtraIO.extra_name_out(sram_index))
        conn_extra.inputFromUInt(data_out_u)
      }
    }
  }
}

object FoundrySRAMExtraConnector {
  def apply(): FoundrySRAMExtraConnector = FoundrySRAMExtraIO.addConnector()
}
