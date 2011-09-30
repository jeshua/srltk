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
import scala.collection.mutable.ArrayBuffer
import srltk.api.agent._

class TabularTD(val alpha: Double, val lambda: Double, val gamma: Double, initialValue: Double = 0)
  extends LearnerV {

  //mutable state
  var initialized: Boolean = false
  var V: Array[Double] = null
  var z: Array[Double] = null
  var numStates: Int = 0

  def value(o: Observation): Double = {
    val state = o.state.asInstanceOf[TabularState].index;
    V(state);
  }

  //imprint learner with example observation and action
  override def onImprint() = {
    //initialize theta with size of imprinted observation
    if (!initialized) {
      val state = imprintedO().state.asInstanceOf[TabularState]
      numStates = state.numStates
      V = new Array[Double](numStates)
      z = new Array[Double](numStates)
      println(numStates)
      initialized = true
    }
  }

  //update value function from observed transition
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation) = learn(otm1, ot)
  override def learn(o1: Observation, a1: Action, o2: Observation) = learn(o1, o2)
  def learn(o1: Observation, o2: Observation) {

    //calculate the TD error
    val delta = o2.reward + gamma * value(o2) - value(o1)
    val s1 = o1.state.asInstanceOf[TabularState].index
    for (i <- 0 until numStates) {
      z(i) = if (i == s1) 1 else gamma * lambda * z(i)
      V(i) += alpha * delta * z(i)
    }
  }
}
