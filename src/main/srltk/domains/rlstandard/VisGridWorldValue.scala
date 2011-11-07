package srltk.domains.rlstandard

import srltk.domains.rlstandard.GridWorldTests.Policy1
import srltk.domains.rlstandard._
import srltk.vis.ValueFunctionVisualizer2D
import srltk.utils.Bounds2D
object VisGridWorldValue {
  def main(args: Array[String]): Unit = {
    val p = new Policy1(1)
    def v(x: Double, y: Double) = p.V2D(x.toInt, y.toInt)
    val visualizer =
      new ValueFunctionVisualizer2D(new Bounds2D(0, GridWorld.width, 0, GridWorld.height), v, true)

  }
}
