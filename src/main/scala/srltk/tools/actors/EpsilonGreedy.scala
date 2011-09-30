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
package srltk.tools.actors

import srltk.api.domain.Action
import srltk.api.domain.Observation
import srltk.api.domain.Action
import scala.util.Random
import scalala.tensor.Vector
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.tensor._
import scala.math
import srltk.api.agent.Imprintable
import srltk.api.agent.CanAct

class EpsilonGreedy(
  var epsilon: Double,
  Q: (Observation, Action) => Double,
  rand: Random) extends CanAct with Imprintable {

  override def onImprint() = ()

  var tmpEpsilon = epsilon
  def disableExploration() = {tmpEpsilon = epsilon; epsilon = 0;}
  def enableExploration() = epsilon = tmpEpsilon
  
  //note, ActorDA implements Imprintable so we implicitly convert ints to actions
  def act(o: Observation): Action = {
    if (rand.nextDouble() < epsilon)
      rand.nextInt(numActions);
    else {
      //tie breaker max
      val vals = for (i <- 0 until numActions) yield Q(o, i)
      val max = vals.reduceLeft(math.max _)
      //println("max = "+max)
      val maxs = for (i <- 0 until numActions; if vals(i) == max) yield i
      if (max.isNaN) throw new IllegalArgumentException("Values contain Nans")
      val index = maxs(rand.nextInt(maxs.length))
      index
    }
  }
}
