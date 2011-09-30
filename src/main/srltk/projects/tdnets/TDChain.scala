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
 ***************************************************************************** */
package srltk.projects.tdnets
import srltk.api.agent._
import srltk.api.domain._
import scala.collection.mutable.HashMap
import srltk.projects._
/**
 * TD Chain is a simple TD net with a chain of nodes each
 * predicting the next node's prediction at the following timestep
 */
class TDChain(root: InternalNodePrototype) 
  extends CanLearn with Imprintable {
  var learner : TDNetLearner = new TDNetLearner(root)
  var net : TDNet = null
  var lastRMSErrors: List[Double] = Nil
  def nodes() = net.internalNodes
  def onImprint() = {learner.imprint(imprintedO,imprintedA);net=learner.net;}

  val predictions = new HashMap[(Int,Int),Feats]
  var timestep = 0
  def learn(otm1: Observation, atm1: Action, ot: Observation): Unit = {      
	  val nodes = net.internalNodes.toList 
	  for(i <- 0 until nodes.size; if(!nodes(i)._2.isInstanceOf[LeafNode]))
		  predictions((i,timestep+(nodes.size-i-1))) = nodes(i)._2.state.yT
	  learner.learn(otm1,atm1,ot)

	  
	  //TD errors
	  def getErr(n : Node) : Double = 
		  n match {
		  case l : InternalNode => l.lastError
		  case _ => 0.0}
	  
	  
	  var errors : List[Double] = Nil
	  for(i <- 0 until nodes.length)
	  {
		  val e = if(predictions.contains(i,timestep))
		  {
			  scala.math.sqrt(((predictions(i,timestep) - ot.features) :^ 2).sum)
		  }
		  else 0
		  errors = e :: errors;  

	  }    
	  lastRMSErrors = errors.reverse

	  this.timestep = timestep+1
  }
}

object TDChain {
	import LeafNodes._
  def apply(k: Int, alpha : Double, ex: FeatureTransform): TDChain = {
  
    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim)

    val nodes = for (i <- 0 until k) yield new InternalNodePrototype("P(t+" + (k-i)+")")
    for (i <- 0 until (k - 1))
      nodes(i) predicts nodes(i + 1) withAnswerPredictor td observes (ObservationNode,ex) 

      nodes(k - 1) predicts ObservationNode withAnswerPredictor td observes (ObservationNode,ex)

      new TDChain(nodes(0))
  }
}
