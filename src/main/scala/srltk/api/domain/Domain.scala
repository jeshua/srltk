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
package srltk.api.domain
import java.awt.Dimension
import java.awt.Graphics2D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class Domain(
		var state: State,
		val rewardFunction: (State, Action, State) => Double,
		val renderer: StateRenderer) {
  
	def exampleAction() = state.exampleAction
	def exampleObservation() = state.exampleObservation
	def numActions = state.exampleAction.numActions
	def obsDim = exampleObservation().features.length	
	def act(a : Action) : Observation = {
	  state = state.successor(a)
	  state.observation()
	}

}

//renders an environment state
trait StateRenderer {
  def render(state: State, g2d: Graphics2D, d: Dimension): Dimension
  val predictions = new ListBuffer[State]
  def addPrediction(state: State) = predictions += state
  def clearPredictions() = predictions.clear
}

//a state class retains all stateful information for a domain
//and has a successor function
abstract class State {
  def getInitial(): State
  def successor(action: Action): State
  def observation(): Observation
  def absorbing: Boolean
  def exampleAction(): Action
  def exampleObservation(): Observation
}

