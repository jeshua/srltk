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
package srltk.api.agent

import srltk.api.domain.Observation
import srltk.api.domain.Action
import srltk.api.agent._

trait CanLearn extends Imprintable{
  def learn(ot: Observation) : Unit =  ()
  def learn(otm1 : Observation , atm1: Action, ot: Observation) : Unit //only one required
  def learn(otm2 : Observation, atm2 : Action, otm1 : Observation, atm1 : Action, ot : Observation) : Unit = ()
}
