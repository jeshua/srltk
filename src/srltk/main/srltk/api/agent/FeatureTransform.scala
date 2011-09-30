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
package srltk.api.agent
import srltk.api.domain.Observation
import scalala.tensor.VectorCol

trait FeatureTransform {
  def <=(o: Observation): Observation = { new Observation(o, <=(o.features)); }
  def <=(input: VectorCol[Double]): VectorCol[Double]
  def length: Int
  val isSparse = false
}

class IdentityTransform(outDim : Int) extends FeatureTransform {
  def <=(input: VectorCol[Double]): VectorCol[Double] = input
  def length: Int = outDim
}
object IdentityTransform { def apply(d: Int) = new IdentityTransform(d) }
