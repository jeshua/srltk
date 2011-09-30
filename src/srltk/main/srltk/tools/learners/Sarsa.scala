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
import srltk.api.agent._

import scalala.tensor.sparse._
import scalala.tensor.mutable._
import srltk.api.agent._

class Sarsa(val alpha0: Double,
  val lambda0: Double,
  val gamma0: Double,
  initialValue: Double, rng: scala.util.Random)
  extends LearnerQ(rng) {
  val TD = new TD(alpha0, lambda0, gamma0, initialValue)

  //create phi_sa
  private def phiSA(o: Observation, a: Action) = {
    val phi = SparseVector.zeros[Double](o.features.length * a.numActions)
    val start = o.features.length * a.index
    o.features foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    new Observation(phi, o.reward, o.state)
  }

  def getQ(o: Observation, a: Action) = TD.value(phiSA(o, a))

  //learn on each sarsa
  def learn(otm1: Observation, atm1: Action, ot: Observation) = ()
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation) =
    TD.learn(phiSA(otm2, atm2), null, phiSA(otm1, atm1));

  //carry imprint call to TD child object
  override def onImprint() =
    {
      TD.imprint(phiSA(imprintedO(), imprintedA()), imprintedA())
    }

}
