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
package srltk.projects.tdnets.test

import srltk.projects.tdnets._

object TDNetExamples {
  import LeafNodes._

  def main(args : Array[String]) : Unit= {
    val alpha = 0.01
    val gamma = 0.99
    val lambda = 0
    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, lambda, inDim, outDim)
    
    val predictor = new InternalNodePrototype("Predictor")
    predictor predicts ObservationNode withAnswerPredictor td observes ObservationNode
    val net0 = new TDNetLearner(predictor)
    net0.net.showDot("/home/jeshua/tdnet0.jpg")
    
    
    /*val valueFunction = new InternalNodePrototype("V")
    def discountedSum(r: Double, v: Double) = toFeats(r + gamma * v)
    valueFunction predicts (discountedSum _, List(RewardNode, valueFunction)) withAnswerPredictor td 
    val net = new TDNet(valueFunction)
    net.showDot("/home/jeshua/tdnet1.jpg")*/
    
    
    /*val featSum = new InternalNodePrototype("featSum")
    def discountedFeatSum(f : Array[Feats]) = f(0) + gamma :* f(1)
    featSum predicts (discountedFeatSum _, List(ObservationNode, featSum)) withAnswerPredictor td 
    val net2 = new TDNet(featSum)
    net2.showDot("/home/jeshua/tdnet2.jpg")*/
    
    
    /*valueFunction observes featSum
    val net3 = new TDNet(featSum)
    net3.showDot("/home/jeshua/tdnet3.jpg")*/
    
    val k = 4
    val nodes = for (i <- 0 until k) yield new InternalNodePrototype("P(t+" + (k-i)+")")
    
    for (i <- 0 until (k - 1))
      nodes(i) predicts nodes(i + 1) withAnswerPredictor td 				
    nodes(k - 1) predicts ObservationNode withAnswerPredictor td 
    val net4 = new TDNet(nodes(0),5)
    net4.showDot("/home/jeshua/tdnet4.jpg")
    
    

  }


}
