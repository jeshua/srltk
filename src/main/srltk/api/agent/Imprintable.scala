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
package srltk.api.agent
import srltk.api.domain.Action
import srltk.api.domain.Observation

/**
 * A class with imprintable must be imprinted with an example action and observation 
 */
trait Imprintable {
  protected var _ia: Action = null
  protected var _io: Observation = null
  protected var imprinted: Boolean = false
  def isImprinted() = imprinted
  def imprintedO() =
    {
      if (!imprinted) throw new IllegalArgumentException("Object has not yet be imprinted with an observation.")
      _io
    }
  def imprintedA() =
    {
      if (!imprinted) throw new IllegalArgumentException("Object has not yet be imprinted with an action.")
      _ia
    }

  def imprint(imprintedObservation: Observation, imprintedAction: Action) {
    this.imprinted = true
    this._io = imprintedObservation
    this._ia = imprintedAction
    this.onImprint()
  }

  //required to override
  protected def onImprint()
  protected def numActions: Int = if(imprintedA==null) 0 else imprintedA().numActions
  implicit def intToAction(i: Int): Action = imprintedA().manufacture(i)
}
