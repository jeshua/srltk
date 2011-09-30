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
import scala.collection.mutable.ListBuffer

class History(val maxLength: Int) {
  require(maxLength > 0)

  val observations = new ListBuffer[Observation];
  val actions = new ListBuffer[Action];
  def t = observations.length
  //number of observations in the observation buffer
  def length(): Int = observations.length

  //append observations and actions
  def append(o: Observation): Unit = {
    //can't append nth observation if there aren't n-1 actions
    require(observations.length == actions.length)

    observations.append(o)
    if (observations.length > maxLength) {
      observations.remove(0)
      actions.remove(0)
    }
  }
  def append(a: Action): Unit = {
    //must always have 1 more observation than action
    require(observations.length - 1 == actions.length)
    actions.append(a)
  }

  def clear(): Unit = { observations.clear; actions.clear(); }

  //o_t-n
  def o_t(n: Int = 0): Observation = {
    require(n <= 0)
    observations(length + n - 1)
  }
  //a_t-n
  def a_t(n: Int = 0): Action = {
    require(n <= 0)
    val aInd = length + n - 1
    actions(aInd)
  }
}
