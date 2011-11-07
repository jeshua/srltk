package srltk.test.control

import srltk.domains.rlstandard._
import srltk.driver.SimpleDriver
import srltk.vis.StateViewer
import scala.collection.mutable.ArrayBuffer
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D
import srltk.algs.linear.agents.EGreedySarsa

object TestSarsaBallBounceWorld {
  import TestRunBallBounceWorld._

  def main(args: Array[String]): Unit = {
    val agent = new EGreedySarsa(BallBounceWorld.dd,.01, .01, 0, .99, 0, ex)
    TestRunBallBounceWorld(agent, 1000000)
  }
}
