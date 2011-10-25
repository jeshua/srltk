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

package srltk.api.agent
import srltk.api.domain.Observation
import scalala.tensor.VectorCol
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector

trait FeatureExtractor {
  def extract(o: Observation): Observation = { new Observation(o, <=(o.features)); }
  def extract(feats: VectorCol[Double]): VectorCol[Double]

  def apply(feats: VectorCol[Double]): VectorCol[Double] = extract(feats)
  def <=(o: Observation): Observation = extract(o)
  def <=(feats: VectorCol[Double]): VectorCol[Double] = extract(feats)
  
  def length: Int
  def isSparse = false


  //offset by num actions automatically
  //output is length*numActions
  def extractActionIndicator(numActions : Int, action : Int, feats : VectorCol[Double]): VectorCol[Double] = {
    val totalLen = length*numActions
    val ret =
      if (isSparse)
        SparseVector.zeros[Double](totalLen)
      else
        DenseVector.zeros[Double](totalLen)
    val start = action * length //start at offset
    val end = start + length
    val v = extract(feats)
    v.foreachNonZeroPair((i: Int, f: Double) => {
      ret(start+i) = f
    })
    ret
  }
}

class IdentityTransform(outDim : Int) extends FeatureExtractor {
  def extract(input: VectorCol[Double]): VectorCol[Double] = input
  def length: Int = outDim
}
object IdentityTransform { def apply(d: Int) = new IdentityTransform(d) }
