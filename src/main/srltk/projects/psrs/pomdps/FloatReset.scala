package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder



class FloatReset(val numStates : Int = 5) extends POMDPSimulator {

  val discount = 1d
  val stateNames = Nil
  reset()
  
  val actionNames = List("float", "reset")
  val numActions = actionNames.length

  val observationNames = Nil
  val numObservations = 2

  def startStateProb(state : Int) = 1d / numStates
/*    if(state==0) .7314d 
    else if(state==1) .1964d 
    else if(state==2) .0528d 
    else if(state==3) .0145d 
    else if(state==4) .0049d 
  else 0d*/
  
  //calculate Pr{state2 | state1+action}
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    a match {
    case "reset" => {
      if (state2==0) 1d
      else 0d
    }
    case "float" => {
      val left = scala.math.min(state1+1,numStates-1)
      val right = scala.math.max(state1-1,0)
      if(state2==left) 0.5
      else if(state2==right) 0.5
      else 0d
    }
    case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    val a = actionNames(action)
    a match {
      case "reset" => {        
        if(state1 == 0){ if(obs == 1) 1d else 0d}
        else if(obs == 0) 1d
        else 0d
      }
      case "float" => {
        if(obs == 1) 0d
        else 1d
      }
      case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
