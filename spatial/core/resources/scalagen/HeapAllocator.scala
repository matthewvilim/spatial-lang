import scala.math

case class HeapAllocator(heapSize: UInt, blockSize: UInt) {
  private case class CompleteBinaryTree[T](leafCount: UInt, default: T) {
    private val vector = Array.fill[T](nodeCount)(default)

    case class Node(index: UInt) {
      val level = log2(index + 1).floor
      def left = if (index < leafCount - 1) Option(Node(2 * index + 1)) else None
      def right = if (index < leafCount - 1) Option(Node(2 * index + 2)) else None
      def parent = if (index > 0) Option(Node((index - 1) / 2)) else None
      def sibling = {
        val i = if (index % 2) index - 1 else index + 1
        if (index > 0) Option(Node(i)) else None
      }
      def get = vector(index)
    }

    def root = Node(0)
    def levelCount = log2(nodeCount + 1)
    def nodeCount = leafCount * 2 - 1

    def levelNodes(level: UInt) {
      var levelNodeCount = pow(2, level)
      vector.slice(levelNodeCount - 1, 2 * levelNodeCount).map{ case index => Node(index) }
    }

    def print {
      val levels = 0 to levelCount - 1
      levelsNodes = levels.map{ case level => levelNodes(level) }
      val levelsString = levelsNodes.map{ case nodes =>
        var levelString = ""
        nodes.foreach{ case node => levelString += node.get }
        levelString
      }
      levels.zip(levelsString).foreach{ case (level, string) => dbg(c"Level $level: $string") }
    }
  }

  object AllocState extends Enumeration {
    type AllocState = Value
    // ordering is important for sorting during allocation
    val Split = Value(0)
    val Empty = Value(1)
    val Full = Value(2)
  }

  val invalidAlloc: UInt = ~0

  private def log2(x: Double): = log(x) / log(2)
  private def isPower2(x: UInt): = log2(x).toInt == log2(x)
  private def power2Ceil(x: UInt): = pow(2, log2(x).ceil)

  if (!isPower2(heapSize) || !isPower2(blockSize)) throw new Exception("Heap and block size must be powers of 2")

  val heap = CompleteBinaryTree[AllocState](heapSize / blockSize, Empty)

  def searchEmptyNode(level: UInt, node: Node) = {
    val stopSearch = node.level == level
    // prioritize allocating in split blocks first
    val children = List(node.left, node.right).map{ case Some(n) => n }
    val childrenSearch = children.map{ case n => searchEmptyNode(n) }
    val orderedChildren = children.zip(childrenSearch).collect{ case (n, Some(x)) => (n, x) }.sortBy{_.2.get} 
    node.get match {
      case Full => None
      case Split => if stopSearch None else orderedChildren.first
      case Empty => if stopSearch Some(node) else orderedChildren.first
    }
  }

  def nodeToAddress(node: Node) = node.left match {
    case Some(n) => nodeToAddress(n)
    case None => {
      val childIndex = node.index - (heap.nodeCount - heap.leafCount)
      childIndex * heap.blockSize
    }
  }
 
  def addressToNode(address: UInt, split: Node) = {
    val spitAddress = nodeToAddress(split)
    if (address == splitAddress && split.get == Full) split else {
      if (address <= splitAddress) addressToNode(address, split.left.get) else addressToNode(address, split.right.get) 
    }
  }

  def markUp(node: Node) {
    val siblingState = match node.sibling {
      case Some(s) => Set(node.get, s.get)
      case None => Set(node.get)
    }

    if (siblingState(Split) || siblingState == Set(Full, Empty)) {
      match node.parent {
        case Some(p) => {
          p.get = Split
          markUp(p)
        }
        case None => _
      }
    } else if (siblingState == Set(Full)) {
      match node.parent {
        case Some(p) => {
          p.get = Full
          markUp(p)
        }
        case None => _
      }
    } else if (siblingState == Set(Empty)) {
      match node.parent {
        case Some(p) => {
          p.get = Empty
          markUp(p)
        }
        case None => _
      }
    }
  }

  def alloc(size: UInt) = {
    val blockCount = power2Ceil(size) / blockSize
    val allocLevel = heap.levelCount - log2(blockCount * 2)

    val allocNode = searchEmptyNode(heap.root)
    allocNode match {
      case Some(n) => {
        n.get = Full
        markUp(n)
        nodeToAddress(n)
      }
      case None => invalidAlloc
    }
  }

  def dealloc(address: UInt) = {
    val node = addressToNode(address, heap.root)
    if (node.index >= heap.nodeCount) throw new Exception("Invalid block address")
    node.get = Empty
    markUp(node)
  }
}
