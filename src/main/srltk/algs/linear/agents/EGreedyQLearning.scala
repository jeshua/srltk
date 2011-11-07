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
package srltk.algs.linear.agents
import srltk.common._
import srltk.algs.linear.learners._
import srltk.features.FeatureExtractor

class EGreedyQLearning(
		dd: DomainDescription,
		alpha: Double = 0.05,
		epsilon: Double = 0.1,
		lambda: Double = 0,
		gamma: Double = 0.99,
		initialValue: Double = 0,
		ex : FeatureExtractor = null)   
  extends
  
  EGreedyAgent(
      dd,
      new QLearning(dd.numActions,if(ex==null) dd.obsDim else ex.length,
    		  alpha, lambda, gamma, initialValue),
      epsilon,
      ex)
