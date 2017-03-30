// See LICENSE for license details.

package dspjunctions

import cde._
import chisel3._
import chisel3.util._
import jtag._
import junctions._

case object DspChainIncludeJtag extends Field[Bool]

class DspJtagIO extends JtagIO {
  // val TRST = Input(Bool())
}

trait HasDspJtagParameters {
  implicit val p: Parameters
  val includeJtag = try {
    if (p(DspChainIncludeJtag)) {
      true
    } else {
      false
    }
  } catch {
    case _: ParameterUndefinedException => false
  }
}

trait HasDspJtagIO extends HasDspJtagParameters {
  val p: Parameters
  val jtag = if (includeJtag) {
      Some(new DspJtagIO)
  } else {
    None
  }
}

trait HasDspJtagModule extends HasDspJtagParameters {
  val io: HasDspJtagIO

  val jtagMaster = if (includeJtag) {
    Some(Module(new JtagAxiMaster()))
  } else {
    None
  }

  def jtagCtrlAxiMasters: Seq[NastiIO] = jtagMaster.map(m => Seq(m.io.ctrlAxi)).getOrElse(Seq())
  def jtagDataAxiMasters: Seq[NastiIO] = jtagMaster.map(m => Seq(m.io.ctrlAxi)).getOrElse(Seq())

  def jtagConnect: Unit = (io.jtag, jtagMaster) match {
    case (Some(jtag), Some(master)) =>
      jtag <> master.io.jtag
    case (None, None) =>
    case _ => throw dsptools.DspException("One of jtagMaster and io.jtag existed, but the other did not!")
  }
}

class JtagAxiMaster()(implicit p: Parameters) extends NastiModule()(p) {
  val io = IO(new Bundle {
    val jtag    = new DspJtagIO
    val ctrlAxi = new NastiIO
    val dataAxi = new NastiIO
  })

  val tap = Module(new JtagTwoAxiMasterClocked(io.jtag.TCK.asClock, false.B)) // io.jtag.TRST))
  io.jtag <> tap.io.jtag
  tap.io.control.fsmAsyncReset := false.B

  io.ctrlAxi <> tap.io.ctrlAxi
  io.dataAxi <> tap.io.dataAxi
}

class JtagTwoAxiMasterClocked(modClock: Clock, modReset: Bool)(implicit val p: Parameters)
  extends Module(override_clock=Some(modClock), override_reset=Some(modReset))
  with HasNastiParameters {
  
  val irLength = 4

  val io = IO(new JtagBlockIO(irLength) {
    val ctrlAxi = new NastiIO
    val dataAxi = new NastiIO
  })

  val ctrl_aw_chain = Module(new DecoupledSourceChain(Output(new NastiWriteAddressChannel)))
  val ctrl_w_chain  = Module(new DecoupledSourceChain(Output(new NastiWriteDataChannel)))
  val ctrl_b_chain  = Module(new DecoupledSinkChain(Output(new NastiWriteResponseChannel)))
  val ctrl_ar_chain = Module(new DecoupledSourceChain(Output(new NastiReadAddressChannel)))
  val ctrl_r_chain  = Module(new DecoupledSinkChain(Output(new NastiReadDataChannel)))

  val data_aw_chain = Module(new DecoupledSourceChain(Output(new NastiWriteAddressChannel)))
  val data_w_chain  = Module(new DecoupledSourceChain(Output(new NastiWriteDataChannel)))
  val data_b_chain  = Module(new DecoupledSinkChain(Output(new NastiWriteResponseChannel)))
  val data_ar_chain = Module(new DecoupledSourceChain(Output(new NastiReadAddressChannel)))
  val data_r_chain  = Module(new DecoupledSinkChain(Output(new NastiReadDataChannel)))

  val tapIO = JtagTapGenerator(irLength, Map(
    0 -> ctrl_aw_chain,
    1 -> ctrl_w_chain,
    2 -> ctrl_b_chain,
    3 -> ctrl_ar_chain,
    4 -> ctrl_r_chain,
    5 -> data_aw_chain,
    6 -> data_w_chain,
    7 -> data_b_chain,
    8 -> data_ar_chain,
    9 -> data_r_chain
  ), idcode = None)

  io.jtag <> tapIO.jtag
  io.output <> tapIO.output


  io.ctrlAxi.aw <> ctrl_aw_chain.io.interface
  io.ctrlAxi.w  <> ctrl_w_chain.io.interface
  io.ctrlAxi.b  <> ctrl_b_chain.io.interface
  io.ctrlAxi.ar <> ctrl_ar_chain.io.interface
  io.ctrlAxi.r  <> ctrl_r_chain.io.interface

  io.dataAxi.aw <> data_aw_chain.io.interface
  io.dataAxi.w  <> data_w_chain.io.interface
  io.dataAxi.b  <> data_b_chain.io.interface
  io.dataAxi.ar <> data_ar_chain.io.interface
  io.dataAxi.r  <> data_r_chain.io.interface
}

