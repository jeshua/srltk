package srltk.vis.spacesconnect

import srltk.common._
import srltk.domains._
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

class DummyAgent[T <: SimState[T]](val w: SpacesWorldInterface[T]) extends spaces.framework.agent.Agent {
  def initTrial(exampleAction: Action): Unit = ()
  def initEpisode(initialObservation: Observation): Unit = ()
  def observeStep(previousAction: Action, resultingObservation: Observation): Unit = ()
  def getNextAction(): Action = {
    w.getExampleAction().getAction(1).asInstanceOf[Action]
  }
}

//==================================================

class SpacesWorldInterface[T <: SimState[T]](var state: T, val stateRenderer : SimStateRenderer[T],val num_actions : Int)
  extends SingleAgentWorld with AbstractAgentWorldInterface {
  def this(d : SimDomain[T]) = this(d.state,d.renderer,d.numActions)
  var last_action : Int = 0;
  //converts spaces action to discrete integer action and makes srltk discrete action
  // then updates state variable using successor function
  def takeAction(action: spaces.framework.util.action.Action): Unit = {
    val a = action.asInstanceOf[SpacesActionInterface].srltkAction
    this.last_action = a.n;
    this.state = this.state.successor(a.n)    
  }

  //uses state renderer, if available, to draw to canvas
  def draw(g2d: Graphics2D, size: Dimension): Dimension = {
    if (stateRenderer == null) size
    else stateRenderer.render(last_action, state, g2d, size)
  }

  def generateObservation(): Observation = {
    val o = new DummyObservation()
    o.setEpisodeOver(this.state.isAbsorbing)
    o
  }
  def getExampleAction(): SpacesActionInterface = new SpacesActionInterface(new IntAction(0),num_actions)
  def startNewEpisode(): Unit = this.state = state.getInitial
  def setWorld(w: World): Unit = ()

  def display() {
    val driver = new Driver(this, new DummyAgent(this), this);
    DockingSpacesFrame.run(driver);
  }
}
