package fringe

import chisel3._
import chisel3.util._
import templates.Utils.log2Up
import templates._

class MuxN[T<:Data](val t: T, val numInputs: Int) extends Module {
  val numSelectBits = log2Up(numInputs)
  val io = IO(new Bundle {
    val ins = Input(Vec(numInputs, t.cloneType))
    val sel = Input(Bits(numSelectBits.W))
    val out = Output(t.cloneType)
  })

  io.out := io.ins(io.sel)
}

class MuxNPipe[T<:Data](val t: T, val numInputs: Int, val stages: Int) extends Module {
  val logInputs = log2Up(numInputs)
  val pow2Inputs = scala.math.pow(2, logInputs).toInt
  //assert(stages > 0 && stages <= c)

  val io = IO(new Bundle {
    val ins = Input(Vec(numInputs, t.cloneType))
    val sel = Input(Bits(logInputs.W))
    val en = Input(Bool())
    val out = Output(t.cloneType)
  })

  /*
  def splitMux(input: Vec[T], stages: Int): (MuxN[T], Bits, Bool) = {
    def slice(v: Vec[T], start: Int, end: Int) = {
      Vec(for (i <- start to end) yield v(i))
    }
    stages match {
      case 1 => {
        val m = Module(new MuxN(t, input.length))
        m.io.ins := input
        val c = log2Up(input.length)
        m.io.sel := io.sel(io.sel.getWidth - 1, io.sel.getWidth - c)
        (m, io.sel(io.sel.getWidth - c - 1, 0), io.validIn)
      }
      case _ => {
        val inL = slice(input, 0, (input.length / 2) - 1)
        val inH = slice(input, input.length / 2, input.length - 1)
        val (muxL, selL, vL) = splitMux(inL, stages - 1)
        val (muxH, selH, vH) = splitMux(inH, stages - 1)
        val ffL = Module(new FFType(t))
        ffL.io.in := muxL.io.out
        ffL.io.enable := true.B
        val ffH = Module(new FFType(t))
        ffH.io.in := muxH.io.out
        ffH.io.enable := true.B

        val ffValid = Module(new FFType(vL))
        ffValid.io.in := vL
        ffValid.io.enable := true.B

        val ffSel = Module(new FFType(selL))
        ffValid.io.in := selL
        ffValid.io.enable := true.B

        val m = Module(new MuxN(t, 2));
        m.io.ins := Vec(ffH.io.out, ffL.io.out)
        m.io.sel := ffSel.io.out(ffSel.io.out.getWidth - 1)
        (m, ffSel.io.out((ffSel.io.out.getWidth - 2).max(0), 0), ffValid.io.out)
      }
    }
  }
  */
  def slice(v: Vec[T], start: Int, end: Int) = {
    Vec(for (i <- start to end) yield v(i))
  }

  val muxL = Module(new MuxN(t, pow2Inputs/2))
  muxL.io.ins := slice(io.ins, 0, (pow2Inputs/2) - 1)
  muxL.io.sel := io.sel(io.sel.getWidth - 2, 0)
  val muxH = Module(new MuxN(t, io.ins.length - pow2Inputs/2))
  muxH.io.ins := slice(io.ins, (pow2Inputs / 2), io.ins.length - 1)
  muxH.io.sel := io.sel(io.sel.getWidth - 2, 0)

  val m = Module(new MuxN(t, 2))
  m.io.ins := Vec(Utils.getRetimedStream(muxL.io.out, stages, io.en),
                  Utils.getRetimedStream(muxH.io.out, stages, io.en))
  m.io.sel := Utils.getRetimedStream(io.sel(io.sel.getWidth - 1), stages, io.en)

  io.out := m.io.out
  /*
  val m = Module(new MuxN(t, numInputs))
  m.io.ins := io.ins
  m.io.sel := io.sel
  io.out := Utils.getRetimedStream(m.io.out, stages, io.en)
  */
}

class MuxNReg(val numInputs: Int, w: Int) extends Module {
  val numSelectBits = log2Up(numInputs)
  val io = IO(new Bundle {
    val ins = Input(Vec(numInputs, Bits(w.W)))
    val sel = Input(Bits(numSelectBits.W))
    val out = Output(Bits(w.W))
  })

  // Register the inputs
  val ffins = List.tabulate(numInputs) { i =>
    val ff = Module(new FF(UInt(w.W)))
    ff.io.enable := true.B
    ff.io.in := io.ins(i)
    ff
  }

  val ffsel = Module(new FF(UInt(numSelectBits.W)))
  ffsel.io.enable := true.B
  ffsel.io.in := io.sel
  val sel = ffsel.io.out

  val mux = Module(new MuxN(UInt(w.W), numInputs))
  mux.io.ins := Vec.tabulate(numInputs) { i => ffins(i).io.out }
  mux.io.sel := sel

  // Register the output
  val ff = Module(new FF(UInt(w.W)))
  ff.io.enable := true.B
  ff.io.in := mux.io.out
  io.out := ff.io.out
}

