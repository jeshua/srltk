package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder


class Cheese extends POMDPSimulator {

  val discount = 95d
  val stateNames = Nil
  val numStates = 11
  reset()
  val actionNames = List("N","S","E","W")
  val numActions = actionNames.length

  val observationNames = Nil
  val numObservations = 7

  def startStateProb(state : Int) = if(state!=10) .1d else 0d
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    a match {
    case "N" => {
      if      (state1==0 && state2 == 0) 1d
      else if (state1==1 && state2 == 1) 1d
      else if (state1==2 && state2 == 2) 1d
      else if (state1==3 && state2 == 3) 1d
      else if (state1==4 && state2 == 4) 1d
      else if (state1==5 && state2 == 0) 1d
      else if (state1==6 && state2 == 2) 1d
      else if (state1==7 && state2 == 4) 1d
      else if (state1==8 && state2 == 5) 1d
      else if (state1==9 && state2 == 7) 1d
      else if (state1==10) startStateProb(state2)//reset
      else 0d
    }
    case "S" => {
      if      (state1==0 && state2 == 5) 1d
      else if (state1==1 && state2 == 1) 1d
      else if (state1==2 && state2 == 6) 1d
      else if (state1==3 && state2 == 3) 1d
      else if (state1==4 && state2 == 7) 1d
      else if (state1==5 && state2 == 8) 1d
      else if (state1==6 && state2 == 10) 1d
      else if (state1==7 && state2 == 9) 1d
      else if (state1==8 && state2 == 8) 1d
      else if (state1==9 && state2 == 9) 1d
      else if (state1==10) startStateProb(state2)//reset
      else 0d
    }
    case "E" => {
      if      (state1==0 && state2 == 1) 1d
      else if (state1==1 && state2 == 2) 1d
      else if (state1==2 && state2 == 3) 1d
      else if (state1==3 && state2 == 4) 1d
      else if (state1==4 && state2 == 4) 1d
      else if (state1==5 && state2 == 5) 1d
      else if (state1==6 && state2 == 6) 1d
      else if (state1==7 && state2 == 7) 1d
      else if (state1==8 && state2 == 8) 1d
      else if (state1==9 && state2 == 9) 1d
      else if (state1==10) startStateProb(state2)//reset
      else 0d
    }
    case "W" => {
      if      (state1==0 && state2 == 0) 1d
      else if (state1==1 && state2 == 0) 1d
      else if (state1==2 && state2 == 1) 1d
      else if (state1==3 && state2 == 2) 1d
      else if (state1==4 && state2 == 3) 1d
      else if (state1==5 && state2 == 5) 1d
      else if (state1==6 && state2 == 6) 1d
      else if (state1==7 && state2 == 7) 1d
      else if (state1==8 && state2 == 8) 1d
      else if (state1==9 && state2 == 9) 1d
      else if (state1==10) startStateProb(state2)//reset
      else 0d
    }
    case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    if      (state1==0 && obs == 0) 1d
    else if (state1==1 && obs == 1) 1d
    else if (state1==2 && obs == 2) 1d
    else if (state1==3 && obs == 1) 1d
    else if (state1==4 && obs == 3) 1d
    else if (state1==5 && obs == 4) 1d
    else if (state1==6 && obs == 4) 1d
    else if (state1==7 && obs == 4) 1d
    else if (state1==8 && obs == 5) 1d
    else if (state1==9 && obs == 5) 1d
    else if (state1==10 && obs == 6) 1d
    else 0d
  }
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
