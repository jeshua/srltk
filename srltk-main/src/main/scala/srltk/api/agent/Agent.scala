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

/**
 * The basic agent structure. An agent recieves and observation on each
 * timestep and emits an action when getAction is called
 *
 * I've taken a new approach to agent structure, these choices are motivated
 * by shortcomings in other RL libraries. Namely, much code in agent design
 * is repeated and easily done incorrectly:
 *    1) storing historical actions and observations for the first three
 *       timesteps. For example, SARSA cannot learn until the 3rd timestep
 *       but when implementing it, it is easy to store and update with the wrong
 *       set of actions
 *          to alleviate this, we provide 3 'learn' functions
 *          learn(ot) [called every timestep t]
 *          learn(otm1,atm1,ot) [called every timestep t > 1]
 *          learn(otm2,atm2,otm1,atm1,ot) [called every timestep t > 2]
 *    2) observing actions taking in the world for off policy learning
 *          we solve this by passing the true previous actions on each timestep
 *          rather then whatever was chosen by the agent at the last timestep
 *    3) providing example observations and actions to the agent. In other
 *       architectures it is necessary to initialize an agent with example actions
 *       and observations, or alternatively require the user to handle their own
 *       initialization after the first observation
 *          we address this through the Imprintable trait. An imprintable object
 *          has access to exampleO() and exampleA(). When querying these, an
 *          exception is thrown if the object has not yet been imprinted
 *          by calling imprint(observation,action). The driver should handle
 *          this so there is no need for users to write custom initialization
 *          code. Instead they can check if imprinted by calling isImprinted()
 *          for additional initialization needs, an onImprint() function is provided
 *
 *
 *
 *
 */

abstract class Agent(val extractor: FeatureTransform) extends CanLearn with CanAct {
  def this() = this(null)

  private val history = new History(3)
  protected var timestep = 0
  protected var myLastAction: Action = null

  final def newEpisode() {
    history.clear()
    timestep = 0
  }

  //Called every timestep by driver
  //on first timestep prevAction is expected to be null
  final def observe(prevAction: Action, o: Observation): Unit =
    {
      //append 
      if (prevAction != null)
        history.append(prevAction)
      history.append(if (extractor != null) new Observation(extractor <= o) else o)

      if (history.length >= 1)
        this.learn(history.o_t());
      if (history.length >= 2)
        this.learn(history.o_t(-1), history.a_t(-1), history.o_t());
      if (history.length > 2)
        this.learn(history.o_t(-2), history.a_t(-2),
          history.o_t(-1), history.a_t(-1), history.o_t());

      timestep += 1
    }

  //external action function
  final def getAction(): Action =
    {
      val a = act(history.o_t())
      myLastAction = a
      a
    }

  //allow alternate imprint behavior using feature extractor
  final override def imprint(o: Observation, a: Action) = {
    if (extractor == null) super.imprint(o, a)
    else super.imprint(extractor <= o, a)
    this.onImprint()
  }

  //======================================================================
  // Required Function to Override  

  //INHERITED FROM IMPRINTABLE  

  /**
   * Imprint notification
   * Called when agent is imprinted with an action and observation
   * after imprint, agent can access exampleO() and exampleA()
   *
   * Purpose: initialize members that require knowledge of observation dimensions
   * or action type
   *
   * HELPERS: isImprinted (from Imprintable trait) can be used to determine
   * if imprint has happened yet so there is no need to write extra initialization
   * checks.
   */
  def onImprint()

  //INHERITED FROM LEARNER

  /**
   * Three learn functions
   *  the first is called at the beginning of time and every step thereafter
   *  the second is called after the first transition (i.e. after second observation) and every step thereafter
   *  the third is called every timestep t > 2
   *
   *  otm1 should be read as action at time t-1
   *  atm1 should be read as action at time t-1
   *
   *  NOTE: actions are the true action taken, not necessarily the agent's previous
   *  action returned by act.
   *  HELPERS: you can access this previous action through myLastAction
   */
  override def learn(ot: Observation): Unit = ()
  override def learn(otm1: Observation, atm1: Action, ot: Observation): Unit //only one required to override
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation): Unit = ()

  //INHERITED FROM ACTOR

  /**
   * Act function returns agent's choice
   *
   * note that the observation passed in is for convenience as it is identical
   * to the last observation passed to learn
   */
  def act(ot: Observation): Action
}
