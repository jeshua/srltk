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
package srltk.tools.learners

import srltk.api.domain._
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;
import srltk.api.agent._

class QLearning(val alpha: Double, val lambda: Double, val gamma: Double, initialValue: Double = 0, rng: scala.util.Random)
  extends LearnerQ(rng) {

  //mutable state
  var numFeats: Int = 0
  var theta: scalala.tensor.mutable.Vector[Double] = null
  var z: scalala.tensor.mutable.Vector[Double] = null

  //linear value function
  def value(o: Observation): Double = o.features dot theta

  //create phi_sa
  private def phiSA(o: Observation, a: Action) = {
    val phi = SparseVector.zeros[Double](o.features.length * a.numActions)
    val start = o.features.length * a.index
    o.features foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    new Observation(phi, o.reward, o.state)
  }

  //imprint learner with example observation and action
  override def onImprint() {
    //initialize theta with size of imprinted observation
    numFeats = phiSA(imprintedO(), imprintedA()).features.length
    theta = DenseVector.zeros(numFeats)
    z = DenseVector.zeros(numFeats)
  }

  def getQ(o: Observation, a: Action) = value(phiSA(o, a))

  def learn(otm1: Observation, atm1: Action, ot: Observation) = {

    val phi1 = phiSA(otm1, atm1);
    val phi2 = phiSA(ot, getMaxA(ot));

    //calculate the TD error
    val delta = phi2.reward + gamma * value(phi2) - value(phi1)

    //Update z and theta, note vector operations
    if (lambda > 0) {
      z = phi1.features + z * gamma * lambda
      theta += z * alpha * delta
    } else
      theta += phi1.features * alpha * delta;
  }
}
