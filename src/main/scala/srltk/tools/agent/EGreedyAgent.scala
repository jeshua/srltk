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
package srltk.tools.agent
import srltk.api.agent._
import scala.util.Random
import srltk.tools.learners.Sarsa
import srltk.tools.actors.EpsilonGreedy
import srltk.api.domain.Action
import srltk.api.domain.Observation
import srltk.api.domain.Action
import srltk.api.agent.FeatureExtractor
import srltk.tools.learners.LearnerQ

//simple agent gluing together a learner with e-greedy
class EGreedyAgent(
    val learner : LearnerQ,
    epsilon : Double,
    rng: Random = new Random(),
    ex: FeatureExtractor = null) extends Agent(ex) with HasActor with HasQFunction {

  def getQ(o : Observation, a : Action) = learner.getQ(o,a) 
  def getMaxA(o : Observation) = learner.getMaxA(o)
  
  val actor = new EpsilonGreedy(
    epsilon,
    learner.getQ _,
    rng);
  
  def enableExploration() = actor.enableExploration()
  def disableExploration() = actor.disableExploration()
  
  // choose actor's action
  override def act(o: Observation) =
    actor.act(o);
  // repeated observation
  def learn(otm1: Observation, atm1: Action, ot: Observation): Unit = ()
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation): Unit =
    learner.learn(otm2, atm2, otm1, atm1, ot);
  // pass along imprint
  override def onImprint() =
    {
      actor.imprint(imprintedO, imprintedA)
      learner.imprint(imprintedO, imprintedA)
    }
}
