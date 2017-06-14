import org.virtualized._
import spatial.dsl._

object LoadTest extends SpatialApp {


  type T = Int

  @virtualize
  def main() {
    val ts = 16
    val N = ArgIn[Int]
    setArg(N, 16)

    val d = DRAM[T](N)
    val q = Array.tabulate(N){i => i*2 }
    setMem(d, q)
    Accel {
      val s = SRAM[T](ts)
      s load d
    }
  }
}
