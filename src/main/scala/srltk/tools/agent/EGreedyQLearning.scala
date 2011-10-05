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
import srltk.tools.learners.QLearning

class EGreedyQLearning(
  alpha: Double = 0.05,
  epsilon: Double = 0.1,
  lambda: Double = 0,
  gamma: Double = 0.99,
  initialValue: Double = 0,
  rng: Random = new Random(),
  ex: FeatureExtractor = null) 
  
  extends
  
  EGreedyAgent(
      new QLearning(alpha, lambda, gamma, initialValue, rng),
      epsilon,
      rng,
      ex)
