package srltk.domains

import srltk.common._
import java.awt.Dimension
import java.awt.Graphics2D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

abstract class SimDomain[T <: SimState[T]]
(var state: T,val renderer: SimStateRenderer[T] = null) 
extends Domain[T, IntAction]{
	def numActions : Int;
	def act(a : IntAction)  = {
	  this.state = state.successor(a.n)
	}
	def getState = state
}

//renders an environment state
trait SimStateRenderer[T <: SimState[T]] {
  def render(state: T, g2d: Graphics2D, d: Dimension): Dimension
}

//a state class retains all stateful information for a domain
//and has a successor function
trait SimState[T <: SimState[T]] extends State[T] {
  def getInitial(): T
  def successor(action: Int): T
  def isAbsorbing: Boolean
}
