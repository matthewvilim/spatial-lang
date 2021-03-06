package spatial.codegen.pirgen

import spatial.SpatialExp
import org.virtualized.SourceContext

// PIR operations which need the rest of the Spatial IR mixed in
trait PIRCommonExp extends PIRCommon { self: SpatialExp =>
  type Expr = Exp[_]
  type CUControl = ControlType

  override def isConstant(x:Expr):Boolean = x match {
    case Const(c) => true
    case Param(c) => true 
    case Final(c) => true 
    case _ => false 
  }

  override def extractConstant(x: Expr): ConstReg[AnyVal] = x match {
    case Const(c: BigDecimal) if c.isWhole => ConstReg(c.toInt) 
    case Const(c: BigDecimal) => ConstReg(c.toFloat) 
    case Const(c: Boolean) => ConstReg(c) 

    case Param(c: BigDecimal) if c.isWhole => ConstReg(c.toInt) 
    case Param(c: BigDecimal) => ConstReg(c.toFloat  ) 
    case Param(c: Boolean) => ConstReg(c) 

    case Final(c: BigInt)  => ConstReg(c.toInt)

    case _ => throw new Exception(s"Cannot allocate constant value for $x")
  }

  def isReadInPipe(mem: Expr, pipe: Expr, reader: Option[Expr] = None): Boolean = {
    readersOf(mem).isEmpty || readersOf(mem).exists{read => reader.forall(_ == read.node) && read.ctrlNode == pipe }
  }
  def isWrittenInPipe(mem: Expr, pipe: Expr, writer: Option[Expr] = None): Boolean = {
    !isArgIn(mem) && (writersOf(mem).isEmpty || writersOf(mem).exists{write => writer.forall(_ == write.node) && write.ctrlNode == pipe })
  }
  def isWrittenByUnitPipe(mem: Expr): Boolean = {
    writersOf(mem).headOption.map{writer => isUnitPipe(writer.ctrlNode)}.getOrElse(true)
  }
  def isReadOutsidePipe(mem: Expr, pipe: Expr, reader: Option[Expr] = None): Boolean = {
    isArgOut(mem) || readersOf(mem).exists{read => reader.forall(_ == read.node) && read.ctrlNode != pipe }
  }

  def isBuffer(mem: Expr): Boolean = isSRAM(mem)

  def isGetDRAMAddress(mem:Expr) = mem match {
    case Def(_:GetDRAMAddress[_]) => true
    case _ => false
  }

  def isLocalMem(mem: Expr): Boolean = isReg(mem) || isFIFO(mem) || isStreamIn(mem) || isStreamOut(mem) || isGetDRAMAddress(mem)

  def isRemoteMem(mem: Expr): Boolean = isSRAM(mem)

  def isMem(e: Expr):Boolean = isLocalMem(e) | isRemoteMem(e) 

  def isLocalMemReadAccess(acc: Expr) = acc match {
    case Def(_:RegRead[_]) => true
    case Def(_:FIFODeq[_]) => true
    case Def(_:ParFIFODeq[_]) => true
    case Def(_:StreamWrite[_]) => true
    case Def(_:ParStreamWrite[_]) => true
    case _ => false
  }

  def isLocalMemWriteAccess(acc: Expr) = acc match {
    case Def(_:RegWrite[_]) => true
    case Def(_:FIFOEnq[_]) => true
    case Def(_:ParFIFOEnq[_]) => true
    case Def(_:StreamRead[_]) => true
    case Def(_:ParStreamRead[_]) => true
    case _ => false
  }

  def isLocalMemAccess(acc: Expr) = isLocalMemReadAccess(acc) || isLocalMemWriteAccess(acc)

  def isRemoteMemAccess(acc:Expr) = acc match {
    case Def(_:SRAMLoad[_]) => true
    case Def(_:ParSRAMLoad[_]) => true
    case Def(_:SRAMStore[_]) => true
    case Def(_:ParSRAMStore[_]) => true
    case _ => false
  }

  def isStage(d: Def): Boolean = d match {
    case _:CounterNew => false
    case _:CounterChainNew => false
    case _:RegNew[_] => false
    case _:SRAMNew[_,_] => false
    case _:FIFONew[_] => false
    case _:StreamInNew[_] => false
    case _:StreamOutNew[_] => false
    case _ => true
  }

  def isStage(e: Expr): Boolean = !isFringe(e) && !isControlNode(e) && getDef(e).exists(isStage)

  //Hack Check if func is inside block reduce
  def isBlockReduce(func: Block[Any]): Boolean = {
    func.summary.reads.intersect(func.summary.writes).exists(isSRAM)
  }

  def flattenNDAddress(addr: Exp[Any], dims: Seq[Exp[Index]]) = addr match {
    case Def(ListVector(List(Def(ListVector(indices))))) if indices.nonEmpty => flattenNDIndices(indices, dims)
    case Def(ListVector(indices)) if indices.nonEmpty => flattenNDIndices(indices, dims)
    case _ => throw new Exception(s"Unsupported address in PIR generation: $addr")
  }

  // returns (sym of flatten addr, List[Addr Stages])
  def flattenNDIndices(indices: Seq[Exp[Any]], dims: Seq[Exp[Index]]):(Expr, List[OpStage]) = {
    val cdims:Seq[Int] = dims.map{case Final(d) => d.toInt; case _ => throw new Exception("Unable to get bound of memory size") }
    val strides:List[Expr] = List.tabulate(dims.length){ d =>
      if (d == dims.length - 1) int32(1)
      else int32(cdims.drop(d+1).product)
    }
    var partialAddr: Exp[Any] = indices.last
    var addrCompute: List[OpStage] = Nil
    for (i <- dims.length-2 to 0 by -1) { // If dims.length <= 1 this won't run
      val mul = OpStage(PIRFixMul, List(indices(i),strides(i)), fresh[Index])
      val add = OpStage(PIRFixAdd, List(mul.out, partialAddr),  fresh[Index])
      partialAddr = add.out
      addrCompute ++= List(mul,add)
    }
    (partialAddr, addrCompute)
  }

  def nodeToOp(node: Def): Option[PIROp] = node match {
    case Mux(_,_,_)                      => Some(PIRALUMux)
    case FixAdd(_,_)                     => Some(PIRFixAdd)
    case FixSub(_,_)                     => Some(PIRFixSub)
    case FixMul(_,_)                     => Some(PIRFixMul)
    case FixDiv(_,_)                     => Some(PIRFixDiv)
    case FixMod(_,_)                     => Some(PIRFixMod)
    case FixLt(_,_)                      => Some(PIRFixLt)
    case FixLeq(_,_)                     => Some(PIRFixLeq)
    case FixEql(_,_)                     => Some(PIRFixEql)
    case FixNeq(_,_)                     => Some(PIRFixNeq)
    case e: Min[_] if isFixPtType(e.mR)  => Some(PIRFixMin)
    case e: Max[_] if isFixPtType(e.mR)  => Some(PIRFixMax)
    case FixNeg(_)                       => Some(PIRFixNeg)

    // Float ops currently assumed to be single op
    case FltAdd(_,_)                     => Some(PIRFltAdd)
    case FltSub(_,_)                     => Some(PIRFltSub)
    case FltMul(_,_)                     => Some(PIRFltMul)
    case FltDiv(_,_)                     => Some(PIRFltDiv)
    case FltLt(_,_)                      => Some(PIRFltLt)
    case FltLeq(_,_)                     => Some(PIRFltLeq)
    case FltEql(_,_)                     => Some(PIRFltEql)
    case FltNeq(_,_)                     => Some(PIRFltNeq)
    case FltNeg(_)                       => Some(PIRFltNeg)

    case FltAbs(_)                       => Some(PIRFltAbs)
    case FltExp(_)                       => Some(PIRFltExp)
    case FltLog(_)                       => Some(PIRFltLog)
    case FltSqrt(_)                      => Some(PIRFltSqrt)
    case e: Min[_] if isFltPtType(e.mR)  => Some(PIRFltMin)
    case e: Max[_] if isFltPtType(e.mR)  => Some(PIRFltMax)

    case And(_,_)                        => Some(PIRBitAnd)
    case Or(_,_)                         => Some(PIRBitOr)
    case _                               => None
  }
  def typeToStyle(tpe: ControlStyle):CUStyle = tpe match {
    case InnerPipe      => PipeCU
    case MetaPipe       => MetaPipeCU
    case SeqPipe        => SequentialCU
    case StreamPipe     => StreamCU
    case ForkJoin       => throw new Exception(s"Do not support ForkJoin in PIR")
    case ForkSwitch     => throw new Exception("Do not support ForkSwitch in PIR")
  }

  // HACK
  def bank(mem: Expr, access: Expr) = {
    val pattern = accessPatternOf(access).last
    val stride  = 1

    val pipe = parentOf(access).get
    val bankFactor = getInnerPar(pipe) 

    val banking = pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt, bankFactor)
      case StridedAccess(Exact(a), i) => StridedBanking(a.toInt, bankFactor)
      case OffsetAccess(i, b)         => StridedBanking(1, bankFactor)
      case LinearAccess(i)            => StridedBanking(1, bankFactor)
      case InvariantAccess(b)         => NoBanking
      case RandomAccess               => NoBanking
    }
    banking match {
      case StridedBanking(stride,f) if f > 1  => Strided(stride)
      case StridedBanking(stride,f) if f == 1 => NoBanks
      case NoBanking if bankFactor==1         => NoBanks
      case NoBanking                          => Duplicated
    }
  }

  /*def bank(mem: Expr, access: Expr, iter: Option[Expr]) = {
    //val indices = accessIndicesOf(access)
    val pattern = accessPatternOf(access)
    val strides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})

    def bankFactor(i: Expr) = if (iter.isDefined && i == iter.get) 16 else 1

    if (pattern.forall(_ == InvariantAccess)) NoBanks
    else {
      val ap = pattern.last
      val str = stride.last
      ap match {
        case AffineAccess(Exact(a),i,b) =>
      }

      (pattern.last, stride.last) match {
        case
      }
      val banking = (pattern, strides).zipped.map{case (pattern, stride) => pattern match {
        case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, bankFactor(i))
        case StridedAccess(Exact(a), i) => StridedBanking(a.toInt*stride, bankFactor(i))
        case OffsetAccess(i, b)         => StridedBanking(stride, bankFactor(i))
        case LinearAccess(i)            => StridedBanking(stride, bankFactor(i))
        case InvariantAccess(b)         => NoBanking
        case RandomAccess               => NoBanking
      }}

      val form = banking.find(_.banks > 1).getOrElse(NoBanking)

      form match {
        case StridedBanking(stride,_)    => Strided(stride)
        case NoBanking if iter.isDefined => Duplicated
        case NoBanking                   => NoBanks
      }
    }
  }*/
  def mergeBanking(bank1: SRAMBanking, bank2: SRAMBanking) = (bank1,bank2) match {
    case (Strided(s1),Strided(s2)) if s1 == s2 => Strided(s1)
    case (Strided(s1),Strided(s2)) => Diagonal(s1, s2)
    case (Duplicated, _) => Duplicated
    case (_, Duplicated) => Duplicated
    case (NoBanks, bank2) => bank2
    case (bank1, NoBanks) => bank1
  }

  def getInnerPar(pipe:Expr):Int = pipe match {
    case Def(Hwblock(func,_)) => 1
    case Def(UnitPipe(en, func)) => 1
    case Def(UnrolledForeach(en, cchain, func, iters, valids)) => 
      val Def(CounterChainNew(ctrs)) = cchain
      val ConstReg(par) = extractConstant(parFactorsOf(ctrs.head).head)
      par.asInstanceOf[Int]
    case Def(UnrolledReduce(en, cchain, accum, func, reduce, iters, valids, rV)) =>
      val Def(CounterChainNew(ctrs)) = cchain
      val ConstReg(par) = extractConstant(parFactorsOf(ctrs.head).head)
      par.asInstanceOf[Int]
  }
}
