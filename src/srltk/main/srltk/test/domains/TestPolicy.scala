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
package srltk.test.domains
import srltk.api.domain._
import srltk.api.agent.Agent

//test policy is an agent so we can run it independently 
abstract class TestPolicy extends Agent {
  def getValue(s: State): Double
  def getInitialState(): State
  def getAction(s: State): Action

  //agent functions
  def learn(otm1: Observation, atm1: Action, ot: Observation): Unit = ()
  //choose action from current observation
  def act(o: Observation): Action = getAction(o.state)
  //imprint doesn't do anything -- presumably the policy knows observation and action spaces
  override def onImprint() = ()
}
