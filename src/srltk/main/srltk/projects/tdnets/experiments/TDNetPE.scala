/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
 * 
 * Copyright 2011 jeshua
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package srltk.projects.tdnets.experiments

import scala.util.Random
import srltk.api.agent._
import srltk.api.domain._
import srltk.api.driver._
import srltk.tools.learners._
import srltk.test.domains._
import srltk.test.domains.MountainCar._
import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.tools.features.CMAC
import srltk.vis.ActivePlot
import srltk.test.policyevaluation.Test2DPE
import srltk.test.policyevaluation.TestPuddleWorldPE
import srltk.test.policyevaluation.TestMountainCarPE
import srltk.projects.tdnets._
import srltk.projects._
//======================================================================


class TDNetPE(val gamma : Double, ex : CMAC,
	    val useFeatSum : Boolean = false,
	    val useChain : Boolean = false) extends LearnerV {
  import LeafNodes._
	var learner = createNet(ex)
	var net = learner.net
	var vNode : Node = null
	def createNet(ex: CMAC): TDNetLearner =
    {
      val k = 2
      def td = (inDim: Int, outDim: Int) => new MultivariateTD(.15, 0, inDim, outDim)
      val valueFunction = new InternalNodePrototype("V")
      
      if(useFeatSum)
      {
        val featSum = new InternalNodePrototype("featSum")
        def discountedFeatSum(f : Array[Feats]) = f(0) + gamma :* f(1)
        featSum predicts (List(ObservationNode, featSum),discountedFeatSum _) withAnswerPredictor td
        val ranges = ex.ranges
        val scale = (1.0/(1-gamma/2))
        val newRanges : List[(Double,Double)] =
          for(r <- ranges)
            yield (r._1*scale,r._2*scale);
        val newCMAC = new CMAC(newRanges,ex.bins,ex.numGrids,ex.rng) 
        
        valueFunction observes (featSum,newCMAC)
      }
      
      if(useChain)
      {
        
        val n1 = new InternalNodePrototype("P+2")
        val n2 = new InternalNodePrototype("P+1")
        n1 predicts n2 observes (ObservationNode, ex) observes(n1,ex) observes(n2,ex) withAnswerPredictor td
        n2 predicts ObservationNode observes (ObservationNode, ex) observes(n2,ex) withAnswerPredictor td
        valueFunction observes (n1,ex) observes(n2,ex)
      }

      
      //def discountedSum(r: Double, v: Double) = toFeats(r + gamma * v)
      def discountedSum(f : Array[Feats]) = toFeats(f(0)(0) + gamma * f(1)(0))
      valueFunction predicts (List(RewardNode, valueFunction), discountedSum _)
      valueFunction withAnswerPredictor td observes (ObservationNode,ex)

      val net = new TDNetLearner(valueFunction)
      net
    }
  
    def value(o: Observation): Double =  {      
      if(vNode == null)  0
      else vNode.getY(o)(0)
    }
	
	override def learn(otm1 : Observation, atm1 : Action, ot: Observation)
	{
	  learner.learn(otm1,null,ot,null,null)
	}
	def onImprint : Unit= {
	    learner.imprint(imprintedO,imprintedA)
	    vNode  = net.internalNodes("V")
	}
}



object TDNetPE {


  var sum : Double = 0
  val step : Double = 100
  val plotStep : Double = 15
  var plot : ActivePlot = null
  var datapoints : List[(Int,Double)] = Nil
  var tdPerformance : List[(Int,Double)] = Nil
  var tdNetPerformance : List[(Int,Double)] = Nil
  
  def updatePlot(timestep : Int, error : ()=>Double, plot : ActivePlot) {
  this.plot = plot
  if(timestep % step == 0) {
    sum = sum + error()
    if(timestep/step % plotStep == plotStep-1) {
      plot.addPoint(timestep,sum/step)
      datapoints ::= (timestep,sum/step)
      sum = 0
    }
  }
  }
  
  def main(args: Array[String]): Unit = {
    //val tester = new TestPuddleWorldPE()
    val tester = new TestMountainCarPE()
    this.plot = new ActivePlot("","Timesteps","RMSE")
    tester.plot = plot
    plot.display()
    plot.newDataset("TD")
    plot.setLocation(0,500)


    this.datapoints = Nil
    def lf(gamma: Double) = new TDNetPE(gamma,tester.ex)
    tester.setUpdateVisFunction(updatePlot _)
    tester.run(lf,60000,false)
    /*if(tdPerformance == Nil)
        tdPerformance = datapoints;
      else{
        var i = -1
        tdPerformance = tdPerformance.map((v:(Int,Double)) => 
        {i+=1;(v._1,v._2+(datapoints(i)._2 - v._2)/(iter+1))})
      }*/

    this.datapoints = Nil
    plot.newDataset("TDNet")
    def lf2(gamma: Double) = new TDNetPE(gamma,tester.ex,true)
    tester.run(lf2,60000,false)




  }
}
