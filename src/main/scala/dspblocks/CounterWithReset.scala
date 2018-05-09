// See LICENSE for license details.

package dspblocks

import chisel3._

object CounterWithReset {
  def apply(cond: Bool, n: Int, sync_reset: Bool, comb_reset: Bool = false.B, sync_reset_val: UInt = 0.U, comb_reset_val: UInt = 0.U): (UInt, Bool) = {
    assert(sync_reset_val <= (n-1).U, "Sync reset value must be within range of counter")
    assert(comb_reset_val <= (n-1).U, "Comb reset value must be within range of counter")
    val c = chisel3.util.Counter(cond, n)
    val out = Wire(UInt())
    out := c._1
    if (n > 1) { 
      val comb_reset_next = Mux(comb_reset_val === (n-1).U, 0.U, comb_reset_val +& 1.U)
      when (sync_reset) { c._1 := sync_reset_val } 
      when (comb_reset) { c._1 := comb_reset_next; out := comb_reset_val }
    }
    (out, c._2)
  }
}
