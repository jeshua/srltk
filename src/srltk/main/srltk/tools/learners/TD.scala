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

class TD(val alpha: Double, val lambda: Double, val gamma: Double, initialValue: Double = 0, beta: Double = 0)
  extends LearnerV {
  require(lambda == 0 || beta == 0)

  //mutable state
  var numFeats: Int = 0
  var theta: scalala.tensor.mutable.Vector[Double] = null
  var z: scalala.tensor.mutable.Vector[Double] = null
  //for gradient td
  var w: scalala.tensor.mutable.Vector[Double] = null

  //linear value function
  override def value(o: Observation): Double = {o.features dot theta}

  //imprint learner with example observation and action
  override def onImprint() {
    //initialize theta with size of imprinted observation
    numFeats = imprintedO().features.length
    theta = DenseVector.zeros(numFeats)
    z = DenseVector.zeros(numFeats)
    //initialize w vector if we're doing gradient TD
    w = if (beta > 0) DenseVector.zeros(numFeats) else null
  }

  //update value function from observed transition
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation) = learn(otm1, ot)
  override def learn(o1: Observation, a1: Action, o2: Observation) = learn(o1, o2)
  def learn(o1: Observation, o2: Observation) {

    //calculate the TD error
    val delta = o2.reward + gamma * value(o2) - value(o1)

    //gradient TD
    if (beta > 0) {
      val d: Double = (o1.features dot w)
      w = (o1.features * (delta - d) * beta)
      theta += (o1.features * delta - o2.features * gamma * d) * alpha
    } else {
      //Update z and theta, note vector operations
      if (lambda > 0) {
        z = o1.features + z * gamma * lambda
        theta += z * alpha * delta
      } else {
        val ad = alpha * delta
        o1.features.foreachNonZeroPair((k: Int, v: Double) =>
          theta(k) += v * ad)

      }
      //println("Mean of theta: "+theta.mean+" alpha "+alpha+" length of features "+o1.features.length+" mean of z is "+z.mean+" reward "+o2.reward+" delta "+delta+" gamma "+gamma + " lambda "+lambda)
      System.out.flush()
    }

    if (o2.absorbing)
      z = DenseVector.zeros(numFeats)
  }
}
