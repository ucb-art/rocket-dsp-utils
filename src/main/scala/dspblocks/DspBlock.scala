// See LICENSE for license details

package dspblocks

import cde._
import chisel3._
import dspjunctions._
// import junctions would import dsptools.junctions._
import _root_.junctions._
import uncore.tilelink._
import uncore.converters._
import rocketchip._
import rocketchip.PeripheryUtils
import diplomacy._
import testchipip._
//import dsptools.Utilities._
import scala.math._
import ipxact._

case object DspBlockId extends Field[String]
case class DspBlockKey(id: String) extends Field[DspBlockParameters]

case class DspBlockParameters (
  inputWidth: Int,
  outputWidth: Int
)

trait HasDspStreamParameters {
  implicit val p: Parameters
  // def baseAddr: Int = p(BaseAddr(p(DspBlockId)))
  def dspBlockExternal = p(DspBlockKey(p(DspBlockId)))
  def inputWidth  = dspBlockExternal.inputWidth
  def outputWidth = dspBlockExternal.outputWidth
  def id: String = p(DspBlockId)
}

// uses DspBlockId
case class GenKey(id: String) extends Field[GenParameters]

trait GenParameters {
  def genIn [T <: Data]: T
  def genOut[T <: Data]: T = genIn[T]
  def lanesIn: Int
  def lanesOut: Int = lanesIn
}

trait HasGenParameters[T <: Data, V <: Data] extends HasDspStreamParameters {
  def genExternal            = p(GenKey(p(DspBlockId)))
  def genIn(dummy: Int = 0)  = genExternal.genIn[T]
  def genOut(dummy: Int = 0) = genExternal.genOut[V]
  def lanesIn                = genExternal.lanesIn
  def lanesOut               = genExternal.lanesOut
  override def inputWidth    = lanesIn * genIn().getWidth
  override def outputWidth   = lanesOut * genOut().getWidth
  // todo some assertions that the width is correct
}

trait HasGenDspParameters[T <: Data, V <: Data] extends HasDspStreamParameters with HasGenParameters[T, V] {
  def portSize[U <: Data](lanes: Int, in: U): Int = {
    val unpadded = lanes * in.getWidth
    val topad = (8 - (unpadded % 8)) % 8
    unpadded + topad
  }
  abstract override def inputWidth     = portSize(lanesIn,  genIn())
  abstract override def outputWidth    = portSize(lanesOut, genOut())
}

trait DspStreamIO {
  def inputWidth: Int
  def outputWidth: Int
  val in  = Input( ValidWithSync(UInt(inputWidth.W)))
  val out = Output(ValidWithSync(UInt(outputWidth.W)))
}

trait SCRFileIO {
  implicit val p: Parameters
  val axi = Flipped(new NastiIO())
}

trait DspBlockIO extends DspStreamIO with SCRFileIO

class BasicDspStreamIO()(implicit val p: Parameters) extends Bundle with HasDspStreamParameters with DspStreamIO {
  override def cloneType: this.type = new BasicDspStreamIO()(p).asInstanceOf[this.type]
}

class BasicDspBlockIO()(implicit override val p: Parameters) extends BasicDspStreamIO with SCRFileIO {
  override def cloneType: this.type = new BasicDspBlockIO()(p).asInstanceOf[this.type]
}

trait HasBaseAddr {
  private var _baseAddr: BigInt = BigInt(0)
  def baseAddr: BigInt = _baseAddr
  def setBaseAddr(base: BigInt): Unit = {
    _baseAddr = base
  }
}
trait HasAddrMapEntry {
  val p: Parameters
  private def addrMapEntryName = p(DspBlockId)
  private def addrMapEntrySize = BigInt(1 << 8)
  def addrMapEntry = AddrMapEntry(addrMapEntryName,
    MemSize(addrMapEntrySize, MemAttr(AddrMapProt.RW))
    )
}

trait HasSCRBuilder {
  val p: Parameters
  private val scrName = p(DspBlockId)
  val scrbuilder = new SCRBuilder(scrName)
}

abstract class DspStream()(implicit val p: Parameters) extends LazyModule with HasDspStreamParameters {
  override def module: DspStreamModule    
}

abstract class DspBlock()(implicit override val p: Parameters) extends DspStream with HasBaseAddr 
    with HasAddrMapEntry with HasSCRBuilder {
  override def module: DspBlockModule

  def size = scrbuilder.controlNames.length + scrbuilder.statusNames.length

  addStatus("uuid")
  addControl("Wrapback")

  def addControl(name: String, init: UInt = null) = {
    scrbuilder.addControl(name, init)
  }
  def addStatus(name: String) {
    scrbuilder.addStatus(name)
  }

}

// TODO: don't duplicate pack/unpack 
abstract class DspStreamModule(val outer: DspStream, b: => Option[Bundle with DspStreamIO] = None)
  (implicit val p: Parameters) extends LazyModuleImp(outer) with HasDspStreamParameters {
  val _io = IO(b.getOrElse(new BasicDspStreamIO))
  val io: Bundle with DspStreamIO = _io.asInstanceOf[Bundle with DspStreamIO]

  def unpackInput[T <: Data](lanes: Int, genIn: T) = {
    val i = Wire(ValidWithSync(Vec(lanes, genIn.cloneType)))
    i.valid := io.in.valid
    i.sync  := io.in.sync
    val w = i.bits.fromBits(io.in.bits)
    i.bits  := w
    i
  }
  def unpackOutput[T <: Data](lanes: Int, genOut: T) = {
    val o = Wire(ValidWithSync(Vec(lanes, genOut.cloneType)))
    io.out.valid := o.valid
    io.out.sync  := o.sync
    io.out.bits  := o.bits.asUInt
    o
  }
}

abstract class DspBlockModule(override val outer: DspBlock, b: => Option[Bundle with DspStreamIO with SCRFileIO] = None)
  (implicit override val p: Parameters) extends DspStreamModule(outer, b) {
  override val io: Bundle with DspStreamIO with SCRFileIO = _io.asInstanceOf[Bundle with DspStreamIO with SCRFileIO]

  def addrmap = testchipip.SCRAddressMap(outer.scrbuilder.devName).get

  val baseAddr = outer.baseAddr

  lazy val scr: SCRFile = {
    println(s"Base address for $name is ${outer.baseAddr}")
    val scr_ = outer.scrbuilder.generate(outer.baseAddr)
    val tl2axi = Module(new TileLinkIONastiIOConverter())
    tl2axi.io.tl <> scr_.io.tl
    io.axi <> tl2axi.io.nasti
    scr_
  }

  def control(name: String) = scr.control(name)
  def status(name : String) = scr.status(name)

  val uuid = this.hashCode
  status("uuid") := uuid.U
}

class GenDspStreamIO[T <: Data, V <: Data]()(implicit val p: Parameters)
  extends Bundle with HasGenDspParameters[T, V] with DspStreamIO {
  override def cloneType = new GenDspStreamIO()(p).asInstanceOf[this.type]
}

class GenDspBlockIO[T <: Data, V <: Data]()(implicit override val p: Parameters)
  extends GenDspStreamIO with SCRFileIO {
  override def cloneType = new GenDspBlockIO()(p).asInstanceOf[this.type]
}

abstract class GenDspStreamModule[T <: Data, V <: Data]
  (outer: DspStream)(implicit p: Parameters) extends DspStreamModule(outer, Some(new GenDspStreamIO[T, V]))
  with HasGenDspParameters[T, V]


abstract class GenDspBlockModule[T <: Data, V <: Data]
  (outer: DspBlock)(implicit p: Parameters) extends DspBlockModule(outer, Some(new GenDspBlockIO[T, V]))
  with HasGenDspParameters[T, V]

