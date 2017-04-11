// See LICENSE for license details.

package jtag

import cde._
import chisel3.iotesters._
import chisel3.iotesters.experimental._
import diplomacy._
import dspblocks._
import _root_.junctions._

import org.scalatest._

class JtagAxiSpec extends FlatSpec with Matchers with JtagAxiUtilities {
  val dut = {implicit p: Parameters => {
    val chain = LazyModule(new DspChainWithAXI4SInput)
    chain.ctrlBaseAddr = () => 0L
    chain.dataBaseAddr = () => 0x1000L
    chain.module
  }}

  behavior of "AXI JTAG"

  it should "get the correct idcode" in {
    val p = chainParameters(BlockConnectEverything, BlockConnectEverything)
    test(dut(p), testerBackend=VerilatorBackend) { implicit t => c =>
      c.io.jtag map { case jtag =>
        println("Reset to idle")
        resetToIdle(jtag)
        println("Idle to IR")
        idleToIRShift(jtag)
        println("Shift in the IR goodness")
        irShift(jtag, "1110".reverse, "10??")
        println("IR to Idle")
        irShiftToIdle(jtag)

        println("Idle to DR")
        idleToDRShift(jtag)
        println("Shift in the DR goodness")
        val idcode =
          "0001" +
          "0" * 15 + "1" +
          "00001000010" + "1"
        drShift(jtag, "0" * idcode.length, idcode.reverse)
        println("DR to Idle")
        drShiftToIdle(jtag)
      }
    }
  }

  it should "read uuids out of the ctrl xbar" in {
    val p = chainParameters(BlockConnectEverything, BlockConnectEverything)
    test(dut(p), testerBackend=VerilatorBackend, options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true)
      }) { implicit t => c =>
      implicit val n: HasNastiParameters = c.io.data_axi.ar.bits

      c.io.jtag map { j =>
        axiRead(j, BigInt(0x8L), BigInt(0x12))
        step(100)
        axiRead(j, BigInt(0x10L), BigInt(0xa))
        step(100)
        axiRead(j, BigInt(0x0L), BigInt(0x0))
      }

    }
  }

  it should "write to wrapback on the ctrl xbar" in {
    val p = chainParameters(BlockConnectEverything, BlockConnectEverything)
    test(dut(p), testerBackend=VerilatorBackend, options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true)
      }) { implicit t => c =>
      implicit val n: HasNastiParameters = c.io.data_axi.ar.bits

      c.io.jtag map { j =>
        println("First read 0x0")
        axiRead(j, BigInt(0x0L), BigInt(0x0))
        step(100)

        println("Write")
        axiWrite(j, BigInt(0x0L), BigInt(0x7), id = 1)
        step(100)
        axiWrite(j, BigInt(0x200L), BigInt(0x12), id = 0)
        step(100)

        println("Second read")
        axiRead(j, BigInt(0x0L), BigInt(0x7))
        step(100)

        println("First read 0x200")
        axiRead(j, BigInt(0x200L), BigInt(0x0))
        step(100)

        println("Write")
        axiWrite(j, BigInt(0x200L), BigInt(0x12))
        step(100)

        println("Second read")
        axiRead(j, BigInt(0x200L), BigInt(0x12))
        step(10)
      }
    }
  }

  it should "write to wrapback on the ctrl xbar using the intermediate backend" in {
    val p = chainParameters(BlockConnectEverything, BlockConnectEverything)
    test(dut(p), testerBackend=chisel3.iotesters.experimental.IntermediateBackend, options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true)
      }) { implicit t => c =>
      implicit val n: HasNastiParameters = c.io.data_axi.ar.bits

      c.io.jtag map { j =>
        println("First read 0x0")
        axiRead(j, BigInt(0x0L), BigInt(0x0))
        step(100)

        println("Write")
        axiWrite(j, BigInt(0x0L), BigInt(0x7), id = 1)
        step(100)
        axiWrite(j, BigInt(0x200L), BigInt(0x12), id = 0)
        step(100)

        println("Second read")
        axiRead(j, BigInt(0x0L), BigInt(0x7))
        step(100)

        println("First read 0x200")
        axiRead(j, BigInt(0x200L), BigInt(0x0))
        step(100)

        println("Write")
        axiWrite(j, BigInt(0x200L), BigInt(0x12))
        step(100)

        println("Second read")
        axiRead(j, BigInt(0x200L), BigInt(0x12))
        step(10)
      }
    }
  }

  it should "read out of the sams on the data xbar" in {
    val p = chainParameters(BlockConnectEverything, BlockConnectEverything)
    test(dut(p), testerBackend=VerilatorBackend, options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true)
      }) { implicit t => c =>
      implicit val n: HasNastiParameters = c.io.data_axi.ar.bits

      c.io.jtag map { j =>
        println("First read 0x1000")
        axiRead(j, BigInt(0x1000L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x1008L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x1010L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x1018L), BigInt(0x0), xbar = DataXBar)
        step(10)

        println("Second read 0x2000")
        axiRead(j, BigInt(0x2000L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x2008L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x2010L), BigInt(0x0), xbar = DataXBar)
        step(10)
        axiRead(j, BigInt(0x2018L), BigInt(0x0), xbar = DataXBar)
        step(10)
      }

    }
  }
}
