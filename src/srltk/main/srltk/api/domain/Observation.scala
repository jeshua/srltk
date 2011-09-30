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
package srltk.api.domain

import scalala.tensor;
import scalala.tensor._;

class Observation(
  val features: VectorCol[Double],
  val reward: Double,
  val state: State, //only for testing purposes, obviously not available in real domains
  val absorbing: Boolean = false) {
  def this(features: VectorCol[Double]) =
    this(features, 0, null, false)

  def this(features: Vector[Double], reward: Double, state: State) =
    this(features.asCol, reward, state, false)

  def this(other: Observation) =
    this(other.features, other.reward, other.state, other.absorbing);

  def this(other: Observation, v: VectorCol[Double]) =
    this(v, other.reward, other.state, other.absorbing)
}
