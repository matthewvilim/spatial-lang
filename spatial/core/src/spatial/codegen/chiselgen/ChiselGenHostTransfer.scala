package spatial.codegen.chiselgen

import argon.codegen.chiselgen.ChiselCodegen
import spatial.compiler._
import spatial.nodes._

trait ChiselGenHostTransfer extends ChiselCodegen  {

  // Does not belong in chisel
  // override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
  //   case _ => super.emitNode(lhs, rhs)
  // }

}
