package srltk.projects.psrs.pomdps

import srltk.vis.ActivePlot
import scala.collection.mutable.Queue
import scala.util.Random
import srltk.tools.utils._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FeatureSpec
import scala.util.Random
import org.junit.Test
import org.junit.Assert._
import srltk.projects.psrs._
import scalala.tensor.dense._
import scala.collection.mutable.ArrayBuffer
import scala.math._

@RunWith(classOf[JUnitRunner])
class POMDPSimulatorTest extends FeatureSpec {


  //======================================================================

  feature("tpsr learning tiger pomdp"){
    //test(new TwoStateMDP)
    //test(new Tiger)
    test(new FloatReset)
  }

  def test(pomdp : POMDPSimulator) = {
    val na = pomdp.numActions
    val no = pomdp.numObservations
    val rng = new scala.util.Random(9)
    pomdp.random = rng
    val tests = DiscretePSRUtils.histsOfLength(2, na, no)
    val hists =  DiscretePSRUtils.histsOfLength(2, na, no)

    pomdp.reset() 
    
    val numTimesteps = 10000
    val length = 100

    var thresh = 0.00001
    var err = 0d 
    var llSum = 0d
    var llCount = 0
    var truthLL = 0d
    var randLL = 0d
    var b = pomdp.uniformSteadyState()
    val obs_queue = new Queue[(Int,Int)]
    val state_queue = new Queue[(Int,Int)]
    
    for(t <- 0 until numTimesteps){
      val a_now = rng.nextInt(na)
      val state1 = pomdp.currentState
      val o_now = pomdp.act(a_now)
      val state2 = pomdp.currentState
      obs_queue.enqueue((a_now,o_now))
      state_queue.enqueue((state1,state2))
      if(obs_queue.length >= length){
        //update state
        val sample = obs_queue.front
        val a = sample._1
        val o = sample._2
        val statesample = state_queue.front
        val s1 = statesample._1
        //---
        val s = DenseVectorCol.zeros[Double](pomdp.numStates)
        s(s1) = 1
        val prob = pomdp.beliefPredict(b,obs_queue.toArray)
        val truth = pomdp.beliefPredict(s,obs_queue.toArray)

        if(length==1)
          assertEquals(truth,pomdp.expectedO(s1,a,o),thresh)

        truthLL += log(truth)
        randLL += log((for(i <- 0 until obs_queue.length) yield 1d/no).foldLeft(1d)(_ * _))
        if(prob > 0){
          llCount += 1
          llSum += log(prob)
        }
        obs_queue.dequeue()
        state_queue.dequeue()
        b = pomdp.beliefUpdate(b,a,o)
      }
      
    }      
    val ll = (llSum/llCount)
      val llZeros = numTimesteps - llCount - length + 1
    printf("(ll: %.4f,truthll: %.4f, random: %.4f, zeros %d)\n",
           ll,truthLL/numTimesteps,randLL/numTimesteps,llZeros)
  }
}
