package dspblocks

import util.GeneratorApp
import org.accellera.spirit.v1685_2009.{File => SpiritFile, Parameters => SpiritParameters, _}
import scala.collection.mutable.HashMap
import javax.xml.bind.{JAXBContext, Marshaller}
import java.io.{File, FileOutputStream}
import scala.collection.JavaConverters
import java.util.Collection
import uncore.tilelink._
import java.math.BigInteger
import rocketchip._
import junctions._
import dspjunctions._
import ipxact._
import cde._

class NastiConfig(implicit val p: Parameters) extends HasNastiParameters {}

case class IPXactParameters(id: String) extends Field[Map[String, String]]

object IPXactComponents {
  //private[dspblocks] val _ipxactComponents: scala.collection.mutable.ArrayBuffer[ComponentType] = 
  val _ipxactComponents: scala.collection.mutable.ArrayBuffer[ComponentType] = 
    scala.collection.mutable.ArrayBuffer.empty[ComponentType]
  def ipxactComponents(): Seq[ComponentType] = _ipxactComponents
}

trait HasDspIPXact extends HasIPXact {

  //////////////////////////////////////////////
  //////////// BUS INTERFACES //////////////////
  //////////////////////////////////////////////

  def makeDspBlockInterfaces(mmref: String): BusInterfaces = {
    val streamInInterface = makeAXI4StreamInterface("io_in", "data_in", false)
    val streamOutInterface = makeAXI4StreamInterface("io_out", "data_out", true)
    val axiInterface = makeAXI4Interface(mmref, "io_axi", "axi4_slave", false)

    val busInterfaces = new BusInterfaces
    busInterfaces.getBusInterface().addAll(toCollection(Seq(streamInInterface, streamOutInterface, axiInterface)))
    busInterfaces
  }

  def makeSAMInterfaces(ctrl_mmref: String, data_mmref: String): BusInterfaces = {
    val streamInInterface = makeAXI4StreamInterface("io_in", "data_in", false)
    val ctrlAXIInterface = makeAXI4Interface(ctrl_mmref, "io_axi", "axi4_slave", false)
    val dataAXIInterface = makeAXI4Interface(data_mmref, "io_axi_out", "axi4_data_slave", false)

    val busInterfaces = new BusInterfaces
    busInterfaces.getBusInterface().addAll(toCollection(Seq(streamInInterface, ctrlAXIInterface, dataAXIInterface)))
    busInterfaces
  }

  def makeXbarInterfaces(inputs: Int, outputs: Int, mmref: String, asref: String): BusInterfaces = {
    val inputInterfaces = (0 until inputs).map(i => makeAXI4Interface(s"${mmref}_${i}", s"io_in_${i}", s"io_in_${i}", false, outputs, "io_out"))
    val outputInterfaces = (0 until outputs).map(i => makeAXI4Interface(s"${asref}_${i}", s"io_out_${i}", s"io_out_${i}", true))
    
    val busInterfaces = new BusInterfaces
    busInterfaces.getBusInterface().addAll(toCollection(Seq(inputInterfaces, outputInterfaces).flatten))
    busInterfaces
  }

  def makeSCRInterfaces(mmref: String): BusInterfaces = {
    val ctrlAXIInterface = makeAXI4Interface(mmref, "io_nasti", "axi4_slave", false)

    val busInterfaces = new BusInterfaces
    busInterfaces.getBusInterface().add(ctrlAXIInterface)
    busInterfaces
  }


  //////////////////////////////////////////////
  //////////// Memory Maps /////////////////////
  //////////////////////////////////////////////

  def makeDspBlockMemoryMaps(mmref: String, baseAddress: BigInt, registers: HashMap[String, BigInt]): MemoryMaps = {
    val addrBlock = makeAddressBlock("SCRFile", baseAddress, registers, 64)
    val memoryMaps = new MemoryMaps
    memoryMaps.getMemoryMap().add(makeMemoryMap(mmref, addrBlocks=Seq(addrBlock)))
    memoryMaps
  }

  // TODO: make the data map correct
  def makeSAMMemoryMaps(ctrl_mmref: String, data_mmref: String, ctrl_baseAddress: BigInt, data_baseAddress: BigInt, registers: HashMap[String, BigInt], width: Int): MemoryMaps = {
    val ctrl_addrBlock = makeAddressBlock("SCRFile", ctrl_baseAddress, registers, 64)
    val data_addrBlock = makeAddressBlock("SAMData", data_baseAddress, HashMap(("SAM", data_baseAddress)), width)
    val memoryMaps = new MemoryMaps
    memoryMaps.getMemoryMap().addAll(toCollection(Seq(makeMemoryMap(ctrl_mmref, addrBlocks=Seq(ctrl_addrBlock)), makeMemoryMap(data_mmref, addrBlocks=Seq(data_addrBlock)))))
    memoryMaps
  }

  def makeXbarMemoryMaps(mmref: String, asref: String, inputs: Int, addrMap: AddrMap): MemoryMaps = {
    val memoryMaps = new MemoryMaps
    memoryMaps.getMemoryMap().addAll(toCollection(
      (0 until inputs).map(i => makeMemoryMap(s"${mmref}_${i}", subspaceRefs=addrMap.flatten.zipWithIndex.map{ case(entry, j) => 
        makeSubspaceRef(s"subspacemap_${mmref}_${i}_io_out_${j}", entry.region.start, s"io_out_${j}", s"${asref}_${j}_segment")
      }))
    ))
    memoryMaps
  }

  def makeSCRMemoryMaps(mmref: String, baseAddress: BigInt, registers: HashMap[String, BigInt]): MemoryMaps = {
    val addrBlock = makeAddressBlock("SCRFile", baseAddress, registers, 64)
    val memoryMaps = new MemoryMaps
    memoryMaps.getMemoryMap().add(makeMemoryMap(mmref, addrBlocks=Seq(addrBlock)))
    memoryMaps
  }

  //////////////////////////////////////////////
  //////////// Address Spaces //////////////////
  //////////////////////////////////////////////

  def makeDspBlockAddressSpaces: AddressSpaces = {
    new AddressSpaces
  }

  def makeSAMAddressSpaces: AddressSpaces = {
    new AddressSpaces
  }

  def makeXbarAddressSpaces(ref: String, addrMap: AddrMap): AddressSpaces = {
    val addressSpaces = new AddressSpaces
    addressSpaces.getAddressSpace.addAll(toCollection(
      addrMap.flatten.zipWithIndex.map { case (entry, i) =>
        makeAddressSpace(s"${ref}_${i}", 
         makeAddressSpaceSegments(
           Seq(makeAddressSpaceSegment(s"${ref}_${i}_segment", entry.region.size, entry.region.start))
         )
        )
      }
    ))
    addressSpaces
  }

  def makeSCRAddressSpaces: AddressSpaces = {
    new AddressSpaces
  }


  //////////////////////////////////////////////
  //////////// Model ///////////////////////////
  //////////////////////////////////////////////

  // assumes DSP block, stream in and out and AXI4 control
  def makeDspBlockPorts(bits_in: Int, bits_out: Int)(implicit p: Parameters): ModelType.Ports = {
    val config = new NastiConfig
    val streamInPorts = makeAXI4StreamPorts("io_in", false, bits_in)
    val streamOutPorts = makeAXI4StreamPorts("io_out", true, bits_out)
    val axiPorts = makeAXI4Ports(s"io_axi", false, config)
    val globalPorts = makeClockAndResetPorts

    val ports = new ModelType.Ports
    ports.getPort().addAll(toCollection(globalPorts ++ streamInPorts ++ streamOutPorts ++ axiPorts))
    ports
  }

  // assumes SAM block, stream in and AXI4 control and data out
  def makeSAMPorts(bits_in: Int)(implicit p: Parameters): ModelType.Ports = {
    // TODO: do we need to separate control from data AXI interface configs here?
    val config = new NastiConfig
    val streamInPorts = makeAXI4StreamPorts(s"io_in", false, bits_in)
    val ctrlAXIPorts = makeAXI4Ports(s"io_axi", false, config)
    val dataAXIPorts = makeAXI4Ports(s"io_axi_out", false, config)
    val globalPorts = makeClockAndResetPorts

    val ports = new ModelType.Ports
    ports.getPort().addAll(toCollection(globalPorts ++ streamInPorts ++ ctrlAXIPorts ++ dataAXIPorts))
    ports
  }

  def makeXbarPorts(inputs: Int, outputs: Int)(implicit p: Parameters): ModelType.Ports = {
    val config = new NastiConfig
    val inPorts = (0 until inputs).map(i => makeAXI4Ports(s"io_in_${i}", false, config))
    val outPorts = (0 until outputs).map(i => makeAXI4Ports(s"io_out_${i}", true, config))
    val globalPorts = makeClockAndResetPorts

    val ports = new ModelType.Ports
    ports.getPort().addAll(toCollection(globalPorts ++ (inPorts ++ outPorts).flatten))
    ports
  }

  def makeSCRPorts()(implicit p: Parameters): ModelType.Ports = {
    val config = new NastiConfig
    val AXIPorts = makeAXI4Ports(s"io_axi", false, config)
    val globalPorts = makeClockAndResetPorts

    val ports = new ModelType.Ports
    ports.getPort().addAll(toCollection(globalPorts ++ AXIPorts))
    ports
  }

  //////////////////////////////////////////////
  //////////// Component ///////////////////////
  //////////////////////////////////////////////

  def makeDspBlockComponent(_baseAddress: BigInt, uuid: Int, name: String)(implicit p: Parameters): ComponentType = {
    val id = p(DspBlockId)
    val mmref = s"${name}_mm"
    val gk = try {
      Some(p(GenKey(p(DspBlockId))))
    } catch  {
      case e: ParameterUndefinedException => None
    }
    val dbk = try {
      Some(p(DspBlockKey(p(DspBlockId))))
    } catch {
      case e: ParameterUndefinedException => None
    }
    val (bits_in, bits_out) = (gk, dbk) match {
      case (Some(g), None) => (g.genIn.getWidth * g.lanesIn, g.genOut.getWidth * g.lanesOut)
      case (None, Some(d)) => (d.inputWidth, d.outputWidth)
      case _ => throw dsptools.DspException("Input and output widths could not be found in the Parameters object!")
    }
    val baseAddress = _baseAddress
    val registers = testchipip.SCRAddressMap(p(DspBlockId)).getOrElse(new HashMap[String, BigInt])
    val busInterfaces = makeDspBlockInterfaces(mmref) 
    val addressSpaces = makeDspBlockAddressSpaces
    val memoryMaps = makeDspBlockMemoryMaps(mmref, baseAddress, registers)
    val model = makeModel(makeDspBlockPorts(bits_in, bits_out))
    val parameterMap = HashMap[String, String]()
    parameterMap ++= p(IPXactParameters(p(DspBlockId))) 
    parameterMap ++= List(("uuid", uuid.toString))
    val parameters = makeParameters(parameterMap)
    makeComponent(name, busInterfaces, addressSpaces, memoryMaps, model, parameters)
  }

  def makeSAMComponent(_ctrl_baseAddress: BigInt, _data_baseAddress: BigInt, memDepth: Int, uuid: Int, name: String)(implicit p: Parameters): ComponentType = {
    val id = p(DspBlockId)
    val ctrl_mmref = s"${name}_ctrl_mm"
    val data_mmref = s"${name}_data_mm"
    val dbk = p(DspBlockKey(p(DspBlockId)))
    val bits_in = dbk.inputWidth
    val ctrl_baseAddress = _ctrl_baseAddress
    val data_baseAddress = _data_baseAddress
    val registers = testchipip.SCRAddressMap(p(DspBlockId)).getOrElse(new HashMap[String, BigInt])
    val busInterfaces = makeSAMInterfaces(ctrl_mmref, data_mmref) 
    val addressSpaces = makeSAMAddressSpaces
    val memoryMaps = makeSAMMemoryMaps(ctrl_mmref, data_mmref, ctrl_baseAddress, data_baseAddress, registers, memDepth)
    val model = makeModel(makeSAMPorts(bits_in))
    val parameterMap = HashMap[String, String]()
    parameterMap ++= p(IPXactParameters(p(DspBlockId))) 
    parameterMap ++= List(("uuid", uuid.toString))
    val parameters = makeParameters(parameterMap)
    makeComponent(name, busInterfaces, addressSpaces, memoryMaps, model, parameters)
  }

  def makeXbarComponent(implicit p: Parameters, name: String): ComponentType = {
    val id = p(DspBlockId)
    val mmref = s"${name}_mm"
    val asref = s"${name}_as"
    val addrMap = p(GlobalAddrMap)
    val inputs = p(InPorts)
    val outputs = p(OutPorts)
    val usePortQueues = p(XBarUsePortQueues)
    val busInterfaces = makeXbarInterfaces(inputs, outputs, mmref, asref) 
    val addressSpaces = makeXbarAddressSpaces(asref, addrMap)
    val memoryMaps = makeXbarMemoryMaps(mmref, asref, inputs, addrMap)
    val model = makeModel(makeXbarPorts(inputs, outputs))
    val parameters = makeParameters(HashMap[String, String](
      "usePortQueues" -> usePortQueues.toString))
    makeComponent(name, busInterfaces, addressSpaces, memoryMaps, model, parameters)
  }

  def makeSCRComponent(_baseAddress: BigInt, id: String, name: String)(implicit p: Parameters): ComponentType = {
    val mmref = s"${name}_mm"
    val baseAddress = _baseAddress
    val registers = testchipip.SCRAddressMap(id).getOrElse(new HashMap[String, BigInt])
    val busInterfaces = makeSCRInterfaces(mmref) 
    val addressSpaces = makeSCRAddressSpaces
    val memoryMaps = makeSCRMemoryMaps(mmref, baseAddress, registers)
    val model = makeModel(makeSCRPorts)
    val parameters = makeParameters(HashMap[String, String]())
    makeComponent(name, busInterfaces, addressSpaces, memoryMaps, model, parameters)
  }
}

object DspIPXact extends HasDspIPXact
