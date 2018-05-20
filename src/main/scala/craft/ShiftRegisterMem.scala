// See LICENSE for license details.

package craft

import chisel3._
import chisel3.util._

object ShiftRegisterMem {

  // use_sp_mem = use single port SRAMs? if false, use dual-port SRAMs
  def apply[T <: Data](in: T, n: Int, en: Bool = true.B, use_sp_mem: Boolean = true, name: String = null): T =
  {
    //require(n%2 == 0, "Odd ShiftRegsiterMem not supported yet")

    if (n == 0) {
      in
    } else if (n == 1) {
      RegEnable(in, en)
    } else if (use_sp_mem && n%2 == 0 && n > 2) { // note: double pumping doesn't doesn't work for shift = 2
      val out = Wire(in.cloneType)
      val mem = SyncReadMem(n/2, Vec(in, in))
      if (name != null) {
        mem.suggestName(name)
      }
      val index_counter = Counter(en, n)._1
      val half_counter = index_counter >> 1.U
      val raddr = half_counter + 1.U 
      val waddr = RegEnable(half_counter, (n/2-1).U, en)
      val wen = index_counter(0) && en
      val des = Reg(in.cloneType)
      val ser = Reg(in.cloneType)

      val sram_out = Reg(next=mem.read(raddr, !wen))

      when (wen) {
        mem.write(waddr, Vec(des, in))
        out := ser
      } .otherwise {
        des := in
        out := sram_out(0)
        ser := sram_out(1)
      }
      out
    } else {
      val mem = SyncReadMem(n, in.cloneType)
      if (name != null) {
        mem.suggestName(name)
      }
      val index_counter = Counter(en, n)._1
      val raddr = index_counter
      val waddr = RegEnable(raddr, (n-1).U, en) //next, init, enable
      when (en) {
        mem.write(waddr, in)
      }
      mem.read(raddr, en)
    }
  }
}
