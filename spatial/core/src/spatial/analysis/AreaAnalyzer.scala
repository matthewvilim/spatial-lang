package spatial.analysis

import argon.core._
import argon.nodes._
import spatial.aliases._
import spatial.models._
import spatial.nodes._
import spatial.metadata._
import spatial.utils._
import org.virtualized.SourceContext

import scala.collection.mutable

case class AreaAnalyzer[Area:AreaMetric,Sum<:AreaSummary[Sum]](var IR: State, areaModel: AreaModel[Area,Sum], latencyModel: LatencyModel) extends ModelingTraversal with AreaMetricOps {
  override val name = "Area Analyzer"
  private val NoArea: Area = noArea[Area]

  var totalArea: Sum = _
  var scopeArea: Seq[Area] = Nil
  var savedArea: Area = NoArea
  var isRerun: Boolean = false

  override def init(): Unit = {
    areaModel.init()
    super.init()
  }

  override def rerun(e: Exp[_], blk: Block[_]): Unit = {
    isRerun = true
    super.rerun(e, blk)
    isRerun = false
  }

  override protected def preprocess[S: Type](block: Block[S]): Block[S] = {
    scopeArea = Nil
    super.preprocess(block)
  }

  override protected def postprocess[S: Type](block: Block[S]): Block[S] = {
    val saved = if (isRerun) savedArea else NoArea
    val total = (saved +: scopeArea).fold(NoArea){(a,b) => implicitly[AreaMetric[Area]].plus(a,b) }
    totalArea = areaModel.summarize(total)

    super.postprocess(block)
  }

  def areaOf(e: Exp[_]): Area = areaModel.apply(e, inHwScope, inReduce)
  def requiresRegisters(x: Exp[_]): Boolean = latencyModel.requiresRegisters(x)
  def retimingDelay(x: Exp[_]): Int = if (requiresRegisters(x)) latencyOf(x).toInt else 0


  def bitBasedInputs(d: Def): Seq[Exp[_]] = exps(d).filterNot(isGlobal(_)).filter{e => Bits.unapply(e.tp).isDefined }.distinct

  def pipeDelayLineArea(block: Block[_], par: Int): Area = {
    val latencies = pipeLatencies(block)._1
    val scope = latencies.keySet
    def delayOf(x: Exp[_]): Int = latencies.getOrElse(x, 0L).toInt
    /*
    Alternative (functional) implementation (it's a groupByReduce! plus a map, plus a reduce):
    scope.flatMap{
      case s@Def(d) =>
        val criticalPath = delayOf(s) - latencyOf(s)
        bitBasedInputs(d).flatMap{in =>
          val size = retimingDelay(in) + criticalPath - delayOf(in)
          if (size > 0) Some(in -> size) else None
        }
      case _ => Nil
    }.groupBy(_._1)
     .mapValues(_.map(_._2).max)
     .map{case (e, delay) => areaModel.areaOfDelayLine(delay.toInt, nbits(e), par) }
     .fold(NoArea){_+_}
   */

    val delayLines = mutable.HashMap[Exp[_],Long]()

    scope.foreach{
      case s@Def(d) =>
        val criticalPath = delayOf(s) - latencyOf(s)
        bitBasedInputs(d).foreach{in =>
          val size = retimingDelay(in) + criticalPath - delayOf(in)
          if (size > 0) {
            delayLines(in) = Math.max(delayLines.getOrElse(in, 0L), size)
          }
        }
      case _ => // No inputs so do nothing
    }

    delayLines.map{case (e,len) => areaModel.areaOfDelayLine(len.toInt,nbits(e),par) }.fold(NoArea){_+_}
  }

  def areaOfBlock(block: Block[_], isInner: Boolean, par: Int): Area = {
    val outerArea = scopeArea
    scopeArea = Nil
    visitBlock(block)
    val area = scopeArea.fold(NoArea){_+_}
    scopeArea = outerArea

    if (isInner) {
      val delayArea = pipeDelayLineArea(block, par)
      area.replicate(par, isInner=true) + delayArea
    }
    else {
      area.replicate(par, isInner=false)
    }
  }

  def areaOfCycle(block: Block[_], par: Int): Area = {
    val outerReduce = inReduce
    inReduce = true
    val area = areaOfBlock(block, isInner=true, par)
    inReduce = outerReduce
    area
  }

  def areaOfPipe(block: Block[_], par: Int): Area = areaOfBlock(block, isInner = true, par)

  override protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = {
    val area: Area = rhs match {
      case Hwblock(block, isForever) =>
        savedArea = scopeArea.fold(NoArea){_+_}
        inHwScope = true
        val body = areaOfBlock(block, isInnerControl(lhs), 1)
        inHwScope = false
        body

      case ParallelPipe(en, block) =>
        val body = areaOfBlock(block, isInner = false, 1)
        dbgs(s"Parallel $lhs: ")
        dbgs(s" - Body: $body")
        body + areaOf(lhs)

      case UnitPipe(en, block)     =>
        val body = areaOfBlock(block, isInner = isInnerControl(lhs), 1)
        dbgs(s"UnitPipe: $lhs")
        dbgs(s" - Body: $body")
        body + areaOf(lhs)

      case OpForeach(en, cchain, block, iters) =>
        val P = parsOf(cchain).product
        val body = areaOfBlock(block, isInnerControl(lhs), P)
        dbgs(s"Foreach: $lhs (P = $P)")
        dbgs(s" - Body: $body")
        body + areaOf(lhs)

      case op@OpReduce(en, cchain, accum, map, load, reduce, store, ident, fold, rV, iters) =>
        val P = parsOf(cchain).product
        val mapArea: Area = areaOfBlock(map, isInnerControl(lhs), P) // Map is duplicated P times
        /*
          Some simple math:
          A full binary (reduction) tree is a tree in which every node is either
          a leaf or has exactly two children.
          The number of internal (non-leaf) nodes of a full tree with L leaves is L - 1
          In our case, L is the map's parallelization factor P
          and internal nodes represent duplicates of the reduction function
          The reduction function is therefore duplicated P - 1 times
          Plus the special, tightly cyclic reduction function to update the accumulator
        */
        val treeArea: Area = areaOfBlock(reduce, isInner = true, P - 1)
        val reduceLength = latencyOfPipe(reduce)._1
        val treeDelayArea: Area = reductionTreeDelays(P).map{dly => areaModel.areaOfDelayLine((reduceLength*dly).toInt,op.bT.length,1) }
                                                  .fold(NoArea){_+_}
        val loadArea: Area  = areaOfCycle(load, 1)
        val cycleArea: Area = areaOfCycle(reduce, 1)
        val storeArea: Area = areaOfCycle(store, 1)

        dbgs(s"Reduce: $lhs (P = $P)")
        dbgs(s" - Map:    $mapArea")
        dbgs(s" - Tree:   $treeArea")
        dbgs(s" - Delays: $treeDelayArea")
        dbgs(s" - Cycle:  ${loadArea + storeArea + cycleArea}")

        mapArea + treeArea + treeDelayArea + loadArea + cycleArea + storeArea + areaOf(lhs)

      case op@OpMemReduce(en,cchainMap,cchainRed,accum,map,loadRes,loadAcc,reduce,storeAcc,ident,fold,rV,itersMap,itersRed) =>
        val Pm = parsOf(cchainMap).product
        val Pr = parsOf(cchainRed).product

        val mapArea = areaOfBlock(map,isInnerControl(lhs),Pm)

        val treeArea = areaOfPipe(reduce, 1).replicate(Pm, isInner=true).replicate(Pr, isInner=false)
        val reduceLength = latencyOfPipe(reduce)._1
        val treeDelayArea = reductionTreeDelays(Pm).map{dly => areaModel.areaOfDelayLine((reduceLength*dly).toInt, op.bT.length, 1) }
                                                   .fold(NoArea){_+_}

        val loadResArea = areaOfCycle(loadRes, 1).replicate(Pr, true).replicate(Pm, false)
        val loadAccArea = areaOfCycle(loadAcc, Pr)
        val cycleArea   = areaOfCycle(reduce, Pr)
        val storeArea   = areaOfCycle(storeAcc, Pr)

        dbgs(s"MemReduce: $lhs (Pm = $Pm, Pr = $Pr)")
        dbgs(s" - Map:    $mapArea")
        dbgs(s" - Tree:   $treeArea")
        dbgs(s" - Delays: $treeDelayArea")
        dbgs(s" - Cycle:  ${loadResArea + loadAccArea + cycleArea + storeArea}")
        mapArea + treeArea + treeDelayArea + loadResArea + loadAccArea + cycleArea + storeArea + areaOf(lhs)

      case _ => rhs.blocks.map(blk => areaOfBlock(blk,false,1)).fold(NoArea){_+_} + areaOf(lhs)
    }
    scopeArea = area +: scopeArea
  }

}
