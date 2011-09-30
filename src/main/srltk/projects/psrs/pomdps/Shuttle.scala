package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

class Shuttle extends POMDPSimulator {

  val discount = 95d
  val stateNames = Nil
  val numStates = 8
  /*
   # 0  Docked in LRV
   # 1  Just outside space station MRV, front of ship facing station
   # 2  Space facing LRV
   # 3  Just outside space station LRV, back of ship facing station
   # 4  Just outside space station MRV, back of ship facing station
   # 5  Space, facing LRV
   # 6  Just outside space station LRV, front of ship facing station
   # 7  Docked in MRV
   */
  reset()
  
  val actionNames = List("turnaround","forward","backward")
  val numActions = actionNames.length

  val observationNames = Nil
  val numObservations = 5

  def startStateProb(state : Int) = if(state==7) 1d else 0d //start docked

  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    a match {
    case "turnaround" => {
      if      (state1==0 && state2 == 1) 1d//from lrv dock
      else if (state1==1 && state2 == 4) 1d//from outside mrv forward
      else if (state1==2 && state2 == 5) 1d//from space facing mrv
      else if (state1==3 && state2 == 6) 1d//from outside lrv backward
      else if (state1==4 && state2 == 1) 1d//from outside mrv backward
      else if (state1==5 && state2 == 2) 1d//from space facing lrv
      else if (state1==6 && state2 == 3) 1d//from outside lrv forward
      else if (state1==7 && state2 == 1) 1d//from docked in mrv
      else 0d
    }
    case "forward" => {
      if      (state1==0 && state2 == 4) 1d//from lrv dock
      else if (state1==1 && state2 == 1) 1d//from outside mrv forward
      else if (state1==2 && state2 == 1) 1d//from space facing mrv
      else if (state1==3 && state2 == 2) 1d//from outside lrv backward
      else if (state1==4 && state2 == 5) 1d//from outside mrv backward
      else if (state1==5 && state2 == 6) 1d//from space facing lrv
      else if (state1==6 && state2 == 6) 1d//from outside lrv forward
      else if (state1==7 && state2 == 4) 1d//from docked in mrv
      else 0d
    }
    case "backward" => {
      if      (state1==0 && state2 == 7) 1d //from lrv dock
      else if (state1==1 && state2 == 1) .4d//from outside mrv forward
      else if (state1==1 && state2 == 2) .3d
      else if (state1==1 && state2 == 4) .3d
      else if (state1==2 && state2 == 3) .8d//from space facing mrv
      else if (state1==2 && state2 == 4) .1d
      else if (state1==2 && state2 == 6) .1d
      else if (state1==3 && state2 == 0) .7d//from outside lrv backward
      else if (state1==3 && state2 == 3) .3d
      else if (state1==4 && state2 == 4) .3d//from outside mrv backward
      else if (state1==4 && state2 == 7) .7d
      else if (state1==5 && state2 == 1) .1d//from space facing lrv
      else if (state1==5 && state2 == 4) .8d
      else if (state1==5 && state2 == 5) .1d
      else if (state1==6 && state2 == 3) .3d//from outside lrv forward
      else if (state1==6 && state2 == 5) .3d
      else if (state1==6 && state2 == 6) .4d
      else if (state1==7 && state2 == 7) 1d //from docked in mrv
      else 0d
    }
    case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
      if      (state2==0 && obs == 4) 1d//from lrv dock
      else if (state2==1 && obs == 1) 1d//from outside mrv forward
      else if (state2==2 && obs == 1) .7d//from space facing mrv
      else if (state2==2 && obs == 3) .3d
      else if (state2==3 && obs == 3) 1d//from outside lrv backward
      else if (state2==4 && obs == 3) 1d//from outside mrv backward
      else if (state2==5 && obs == 0) .7d//from space facing lrv
      else if (state2==5 && obs == 3) .3d
      else if (state2==6 && obs == 0) 1d//from outside lrv forward
      else if (state2==7 && obs == 2) 1d//from docked in mrv
      else 0d
  }
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
