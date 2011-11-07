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
package srltk.driver
import srltk.common._
import srltk.domains._

class SimpleDriver(var d: SimDomain, val ag: Agent) {
  var prevAction: Action = null
  val a = new ManagedAgent(ag)
  def step(): (SimState, SimObservation, Action, SimObservation, Boolean) =
    {
      //agent observes current state and then acts
      val obs1 = d.state.observation()
      a.observe(prevAction, obs1)
      val action = a.getAction()
      var newEpisode = false
      prevAction = action

      d.state =
        if (!d.state.absorbing)
          d.state.successor(action); //take action from non-absorbing state, just choose successor
        else {
          val newObs = new SimObservation(obs1.vec, 0, obs1.simState) //observation of absorbing state w/o reward
          a.observe(prevAction, newObs) //so agent observes absorbing state again
          a.newEpisode() //reset agent's history
          prevAction = null
          newEpisode = true
          d.state.getInitial() //return start state
        }
      val obs2 = d.state.observation
      (d.state, obs1, action, obs2, newEpisode)
    }
}
