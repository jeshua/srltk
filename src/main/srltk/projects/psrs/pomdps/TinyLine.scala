package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

/*
 * States, two squares |A B|
 * Actions     : A,R->B, B,L->A
 * Observations:
 *   *,*,A: .8A, .2B
 *   *,*,B: .8B, .2A
 */
 
class TinyLine extends POMDPSimulator {

  val discount = 0.75

  val stateNames = Nil
  val numStates = 4
  reset()
  
  val actionNames = List("left","right")
  val numActions = actionNames.length

  val observationNames = Nil
  val numObservations = 2

  def startStateProb(state : Int) = 1d/numStates
  
  //calculate Pr{state2 | state1+action}
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    a match {
      case "left" => {
        if      (state1 == 1 && state2 == 0) 1d
        else if (state1 == 2 && state2 == 1) 1d
        else if (state1 == 3 && state2 == 2) 1d
        else if (state1 == 0 && state2 == 0) 1d
        else 0d
      }
      case "right" => {
        if      (state1 == 0 && state2 == 1) 1d
        else if (state1 == 1 && state2 == 2) 1d
        else if (state1 == 2 && state2 == 3) 1d
        else if (state1 == 3 && state2 == 3) 1d
        else 0d
      }
      case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    if     (state2 == 0 && obs==0) 0.2
    else if(state2 == 1 && obs==0) 0.4
    else if(state2 == 2 && obs==0) 0.6
    else if(state2 == 3 && obs==0) 0.8
    else if(state2 == 0 && obs==1) 0.8
    else if(state2 == 1 && obs==1) 0.6
    else if(state2 == 2 && obs==1) 0.4
    else if(state2 == 3 && obs==1) 0.2    
    else 0d
  }
  //calculate reward
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
