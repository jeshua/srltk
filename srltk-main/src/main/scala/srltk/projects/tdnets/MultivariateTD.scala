/**
 * *****************************************************************************
 * Scala Reinforcement Learning Toolkit
 *  @author Jeshua Bratman
 *  @email jeshuabratman@gmail.com
 *
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

import scalala.operators.Implicits._
import scalala.tensor._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import srltk.projects._
/**
 *
 */
class MultivariateTD(
  var alpha: Double,
  val lambda: Double,
  val inDim: Int, val outDim: Int, val bias : Boolean = false, val useSigmoid : Boolean = false)
  extends Predictor(inDim, outDim, bias) {

  //mutable state
  val biasInDim = inDim+ (if(bias) 1 else 0)
  val theta: DenseMatrix[Double] =
    DenseMatrix.zeros(outDim, biasInDim)
  var z: VectorCol[Double] = null
  var lastError: Double = 0

  def sigmoid(d : Double) : Double = {
    1/(1+scala.math.exp(-d))
  }
  def dsigmoid(d : Double) : Double = {
    val s = sigmoid(d)
    s * (1-s)
  }
  
  /**
   *
   * @param feats : inDmin x 1
   * @return prediction : outDim x 1
   */
  override def predict(input: Feats): Feats = {
    val in = super.addBias(input)
    if (in.length != biasInDim) throw new IllegalArgumentException("Input dim should be " + inDim + " but is " + in.length)
    
    val ret = (theta * in).asCol // (outDim x inDim) * (inDim x 1)
    //apply sigmoid if set
    if(useSigmoid)
    	for(i <- 0 until ret.length) ret(i) = sigmoid(ret(i))    
    ret
  }

  /**
   *
   * Input corresponding to predictions of parent's output
   * @param observation : inDim x 1  input features at time t
   *
   * Function to compute error of prediction
   * @param error : outDim x 1
   */
  override def learn(input: Feats, error: Feats): Double = {
    require(input.length == inDim)
    val feats = super.addBias(input)    
    this.lastError = error.toList.reduceLeft(scala.math.abs(_) + scala.math.abs(_))
    z = if (lambda <= 0 || z == null) feats else feats + z :* lambda

    //this.theta += (error * z.t) :* alpha;
    error.foreachNonZeroPair((i: Int, er: Double) => {
    	z.foreachNonZeroPair((j: Int, f: Double) => {
    		this.theta(i, j) += alpha * er * (if(useSigmoid) dsigmoid(f) else f)
    	})
      })
    //save the new vector
    this.z = z

    lastError
  }
}

