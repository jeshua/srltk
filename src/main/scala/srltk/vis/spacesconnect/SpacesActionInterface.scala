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
package srltk.vis.spacesconnect
import srltk.api.domain._;

class SpacesActionInterface(val index: Int, val numActions: Int, val a: Action)
  extends spaces.framework.util.action.DiscreteAction {
  def this(action: Action) = this(action.index, action.numActions, action)

  def getValue(): Int = index
  def getValues(): Array[spaces.framework.util.action.DiscreteAction] =
    {
      val t: IndexedSeq[spaces.framework.util.action.DiscreteAction] =
        for (i <- 0 until numActions) yield new SpacesActionInterface(i, numActions, a)
      t.toArray
    }

  def getAction(i: Int): SpacesActionInterface = new SpacesActionInterface(i, numActions, a)
  def srltkAction() = a.manufacture(index);
  override def toString() = srltkAction().name
}
