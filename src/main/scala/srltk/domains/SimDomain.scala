package srltk.domains

import srltk.common._
import java.awt.Dimension
import java.awt.Graphics2D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

abstract class SimDomain
(var state: SimState,val renderer: SimStateRenderer = null) 
extends Domain[SimState, IntAction]{
	def numActions : Int;
	def act(a : IntAction)  = {
	  this.state = state.successor(a)
	}
	def getState = state
}

//renders an environment state
trait SimStateRenderer {
  def render(state: SimState, g2d: Graphics2D, d: Dimension): Dimension
}

//a state class retains all stateful information for a domain
//and has a successor function
trait SimState extends State[SimState] {
  def getInitial(): SimState
  def successor(action: Action): SimState
  def isAbsorbing: Boolean
}
