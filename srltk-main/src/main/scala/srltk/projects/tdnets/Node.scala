/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package srltk.projects.tdnets
import scalala.tensor._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import srltk.api.agent.FeatureTransform
import srltk.api.agent.IdentityTransform
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import srltk.tools.features.CMAC
import srltk.api.domain.Observation
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import srltk.api.domain.Action
import srltk.api.domain.Action
import java.util.IdentityHashMap
import srltk.projects._

class NodeState(
  val timestep: Int,
  //x_t
  val xT: Feats,
  //y^i_t
  val yT: Feats,
  //o^t+1
  val oTp1 : Observation,
  //x_t
  val xTp1: Feats,
  //\tilde{y}^i_{t+1}
  val yTp1: Feats,
  //predictor objects includes weights (mutable state)
  val questionPredictor: Predictor,
  val answerPredictor: Predictor)
  
  
trait Node {
  def name : String
  def getY(obs : Observation) : Feats
  def getYTilde(obs : Observation) : Feats
  def flatten() : List[Node] = flatten(new HashSet[Node])
  def flatten(directory : HashSet[Node]) : List[Node] = {
    if(directory.contains(this)) Nil
    else{
      directory.add(this)
      List(this)
    }
  }
}
  
//================================================================================

class InternalNode(
  _name: String,
  //question network
  val targetNodes: Array[Node], //K
  val targetTransform: MultiFeatureTransform, //K
  val questionPredictor: Predictor, //Z, mu
  //answer network
  val inputNodes: Array[Node], //X
  val inputTransform: MultiFeatureTransform, //X
  val answerPredictor: Predictor, //Y, theta
  //error function between Y and Z
  val error: ErrorFunction,
  //values to determine dimensionality of all inputs/outputs
  val xDim : Int, //dimensionality of input
  val yDim : Int //dimensionality of predictions
  ) extends Node {

  
  def name = _name
  
  var state = new NodeState(
    0,
    //x_t
    DenseVector.zeros[Double](xDim),    
    //y^i_t
    DenseVector.zeros[Double](yDim), 
    //otp1
    null,
    //x_t+1
    DenseVector.zeros[Double](xDim), 
    //\tilde{y}^i_{t+1}
    DenseVector.zeros[Double](yDim), 
    //predictors contain mutable state
    questionPredictor,
    answerPredictor)

  def getY(obs : Observation) = state.yT
  def getYTilde(obs : Observation) = state.yTp1
  
 /**
   * The following functions provide the interface to a node
   *
   * Observe: node observes o_t+1 and calculates x_t+1, y_t, ~y_t+1
   *          returns the modified node state
   * Update : constructs targets z_t and updates network weights based on current weights
   *          returns the modified node state
   *
   * Predict: several functions to extract this node's predictions
   */
  //get most recent prediction
  def predict(): Feats = state.yT
  def predict(o : Observation, a : Action): Feats =  {
      val state1 = this.state
      this.state = observe(o,a)
      val prediction = predict()
      this.state = state1
      prediction      
    }
	
  /**
   * Observes action at time t+1 and uses answer network to
   * update yt, returns modified state
   */
  def observe(otp1: Observation, atp1: Action): NodeState = { 
    val xTp1 = X(otp1)
    new NodeState(
      state.timestep + 1,
      state.xT,
      state.yT,//yT should have been set on previous iteration
      otp1,//otp1
      xTp1, //xT --> xTp1
      Y(xTp1),//calculate tilde yTp1      
      state.questionPredictor,
      state.answerPredictor)
  }

  /**
   * Answer Network
   */
  def X(obs : Observation) : Feats = {
    //get all the input node predictions
    val yts : Array[Feats] = for (n <- inputNodes) yield n.getY(obs)
    	
    //compute input features
    val r = inputTransform(yts)
    if(r.length != xDim) throw new IllegalArgumentException("Input transformation expected to return vector of length = "+xDim+", but instead length = "+r.length)
    r
  }

  def Y(x: Feats): Feats =
    {
      val r = state.answerPredictor.predict(x)
      assert(r.length == yDim)
      r
    }

  var lastError : Double = 0
  def update(): Unit = {
    val k = K(state.oTp1)
    val z = Z(k)
 
    val er1 = if (error == null) SimpleError(z)(state.yT) else error(z)(state.yT)
    lastError = scala.math.sqrt(er1.toList.foldLeft(0d)(_ + scala.math.pow(_,2)))
    state.answerPredictor.learn(state.xT, er1)
    if(questionPredictor != null){//we don't always use a question predictor    
    	state.questionPredictor.learn(k, er1)
    }
    
    this.state = 
    	new NodeState(state.timestep,state.xTp1,Y(state.xTp1),state.oTp1,null,state.yTp1,questionPredictor,answerPredictor)
  }

  /**
   * Question Network
   */
  def K(obs : Observation): Feats = {
    //get all the target node's predictions
    val yts : Array[Feats] = for (n <- targetNodes) yield n.getYTilde(obs)
    //compute question features
    targetTransform(yts)
  }

  def Z(k: Feats): Feats =
    {
    if (questionPredictor == null) k
      else state.questionPredictor.predict(k)
    }

  /**
   * Flatten out children nodes into a list
   */
  override def flatten() : List[Node] = flatten(new HashSet[Node])
  override def flatten(n : HashSet[Node]) : List[Node] = {
    if (n.contains(this)) Nil
    else {
      n.add(this)      
      val t : List[Node] = (for (target <- targetNodes) 
    	  yield target.flatten(n)).toList.flatten
      val c : List[Node] = (for (input <- inputNodes) 
    	  yield input.flatten(n)).toList.flatten
      this :: (t ::: c)
    }
  }
}
//======================================================================

