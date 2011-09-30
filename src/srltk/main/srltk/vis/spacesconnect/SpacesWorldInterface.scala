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

import srltk.api.domain._

import spaces.framework.driver.Driver
import spaces.framework.perception.AbstractAgentWorldInterface
import spaces.framework.perception.AbstractContinuousObservation
import spaces.framework.perception.AbstractFeatureVector
import spaces.framework.perception.AbstractFeatureVector.FeatureVectorEntry
import spaces.framework.perception.Observation
import spaces.framework.ui.DockingSpacesFrame
import spaces.framework.ui.data.GraphingDriverEventsListener.UpdateMode
import spaces.framework.util.action.Action
import spaces.framework.util.action.Action
import spaces.framework.world.World
import spaces.framework.world.SingleAgentWorld
import java.awt.Dimension
import java.awt.Graphics2D

class DummyObservation extends spaces.framework.perception.Observation {}

class DummyAgent(val w: SpacesWorldInterface) extends spaces.framework.agent.Agent {
  def initTrial(exampleAction: Action): Unit = ()
  def initEpisode(initialObservation: Observation): Unit = ()
  def observeStep(previousAction: Action, resultingObservation: Observation): Unit = ()
  def getNextAction(): Action = {
    w.getExampleAction().getAction(1).asInstanceOf[Action]
  }
}

//==================================================

class SpacesWorldInterface(var state: State, val stateRenderer: StateRenderer,
  val exampleAction: srltk.api.domain.Action)
  extends SingleAgentWorld with AbstractAgentWorldInterface {
  //optionally have no draw function
  def this(state: State, exampleAction: srltk.api.domain.Action) =
    this(state, null, exampleAction);
  def this(domain: Domain) =
    this(domain.state, domain.renderer, domain.exampleAction.asInstanceOf[srltk.api.domain.Action])

  //converts spaces action to discrete integer action and makes srltk discrete action
  // then updates state variable using successor function
  def takeAction(action: spaces.framework.util.action.Action): Unit = {
    val a = action.asInstanceOf[SpacesActionInterface].srltkAction
    this.state = this.state.successor(a)
  }

  //uses state renderer, if available, to draw to canvas
  def draw(g2d: Graphics2D, size: Dimension): Dimension = {
    if (stateRenderer == null) size
    else stateRenderer.render(state, g2d, size)
  }

  def generateObservation(): Observation = {
    val o = new DummyObservation()
    o.setEpisodeOver(this.state.absorbing)
    o
  }
  def getExampleAction(): SpacesActionInterface = new SpacesActionInterface(exampleAction)
  def startNewEpisode(): Unit = this.state = state.getInitial
  def setWorld(w: World): Unit = ()

  def display() {
    val driver = new Driver(this, new DummyAgent(this), this);
    DockingSpacesFrame.run(driver);
  }
}
