package amba

import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamEdgeParameters, AXI4StreamMasterPortParameters, AXI4StreamSlavePortParameters}
import freechips.rocketchip.diplomacy.NodeHandle

package object axi4stream {
  type AXI4StreamNode = NodeHandle[
    AXI4StreamMasterPortParameters, AXI4StreamSlavePortParameters, AXI4StreamEdgeParameters, AXI4StreamBundle,
    AXI4StreamMasterPortParameters, AXI4StreamSlavePortParameters, AXI4StreamEdgeParameters, AXI4StreamBundle]

}