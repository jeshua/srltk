/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.domains

import srltk.common._
import java.awt.Dimension
import java.awt.Graphics2D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class SimObservation(
		vec : Traversable[Double],
		reward : Double,
		val simState : SimState = null) extends Observation(vec, reward) {}

abstract class SimDomain(
		var state: SimState,
		val rewardFunction: (SimState, Action, SimState) => Double,
		val renderer: SimStateRenderer = null) extends Domain{
  
	
	def act(a : Action) : Observation = {
	  state = state.successor(a)
	  state.observation()
	}

}

//renders an environment state
trait SimStateRenderer {
  def render(state: SimState, g2d: Graphics2D, d: Dimension): Dimension
  val predictions = new ListBuffer[SimState]
  def addPrediction(state: SimState) = predictions += state
  def clearPredictions() = predictions.clear
}

//a state class retains all stateful information for a domain
//and has a successor function
trait SimState {
  def getInitial(): SimState
  def successor(action: Action): SimState
  def observation(): SimObservation
  def absorbing: Boolean
}
