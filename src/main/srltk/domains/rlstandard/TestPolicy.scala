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
package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._

//test policy is an agent so we can run it independently 
abstract class TestPolicy(dd : DomainDescription) extends Agent(dd) {
  def getValue(s: SimState): Double
  def getInitialState(): SimState
  def getAction(s: SimState): IntAction

  //agent functions
  def observe(otm1: Observation, atm1: Action, ot: Observation): Unit = ()
  //choose action from current observation
  def act(o: Observation): Action = getAction(o.asInstanceOf[SimObservation].simState)
}
