package srltk.test.control

import srltk.features.CMAC
import srltk.domains.rlstandard._
import srltk.algs.linear.agents._
import srltk.algs.linear._
import srltk.common._
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

object TestRunMountainCar
{
  //create mountain car
  val domain = new MountainCar
  //Feature extractor for mountain car
  val ex = new CMAC(
      List((MountainCar.xMin, MountainCar.xMax),
          (MountainCar.xDotMin, MountainCar.xDotMax)),
          List(13, 13), 10)
  
  def apply(agent : FeatAgent,episodes : Int,  visuals :Boolean = true) {
    TestRun(domain,agent,None,Some(episodes),visuals)
  }
}


object TestRunPuddleWorld
{
  val rng = new scala.util.Random()
  //Feature extractor for mountain car
  val domain = new PuddleWorld()
  val ex = new CMAC(
      List((0, 1),
          (0, 1)),
          List(10, 10), 10)
  def apply(agent : FeatAgent,episodes : Int,  visuals :Boolean = true)
  {
    TestRun(domain,agent,None,Some(episodes),visuals)
  }
  def run2(agent : FeatAgent, timesteps: Int,  visuals :Boolean = true)
  {
    TestRun(domain,agent,Some(timesteps),None,visuals)
  }
}


object TestRunBallBounceWorld
{
  val domain = new BallBounceWorld()
  val bin = 10
  val ex = new CMAC(
      List((0, 1),
          (0, 1),
          (0, 1),
          (0, 1),
          (0, scala.math.Pi)),
          List(bin, bin, bin, bin, bin), 8)

  def apply(agent : FeatAgent, timesteps : Int, visuals :Boolean = true) {
    TestRun(domain,agent,Some(timesteps),None,visuals)
  }
}
