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
package srltk.projects.tdnets
import scalala.tensor.mutable.VectorCol
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector
import srltk.projects._
abstract class Predictor(val iDim: Int, val oDim: Int, bias : Boolean = false) {
  def predict(input: Feats): Feats;
  def learn(input: Feats, error: (Feats) => Feats): Double = learn(input, error(predict(input)))
  def learn(input: Feats, error: Feats): Double;
  
  def addBias(input : Feats) : Feats = {
    if(!bias) input
    else
    {
      val o : VectorCol[Double] = 
        if(input.isInstanceOf[SparseVector[_]])
          SparseVector.zeros[Double](input.length+1)
        else
          DenseVector.zeros[Double](input.length+1)
       o(0) = 1
       input.foreachNonZeroPair((k : Int, v : Double) => o(k+1) = v)
       o
    }    
    
  }
  
}

class IdentityPredictor extends Predictor(0, 0) {
  def predict(input: Feats) = input
  def learn(input: Feats, error: Feats) = 0
}
object IdentityPredictor { def apply = new IdentityPredictor }
