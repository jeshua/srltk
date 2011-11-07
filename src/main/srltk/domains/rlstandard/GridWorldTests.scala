package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import GridWorld._

object GridWorldTests {

  //just takes shortest path to the goal, with some randomness
  class Policy1(gamma: Double)
    extends TestPolicy(GridWorld.dd) {

    def V2D(x: Int, y: Int) = getValue(new GridWorldState(x, y))

    def getValue(s: SimState) = {
      val state = s.asInstanceOf[GridWorldState]
      val distance = scala.math.abs(state.x - GridWorld.goalX) + scala.math.abs(state.y - GridWorld.goalY) - 1
      //straight-line distance to goal
      if (distance <= 0) 0
      else if (gamma == 1) -distance
      else -1 * ((1 - scala.math.pow(gamma, distance)) / (1 - gamma))
    }

    def reset() = ()
    def getInitialState() = GridWorld.getInitial

    def getAction(s: SimState) = {
      val state = s.asInstanceOf[GridWorldState]
      val x = state.x
      val y = state.y

      if (x < GridWorld.goalX) GridWorldAction.get("East")
      else if (x > GridWorld.goalX) GridWorldAction.get("West")
      else if (y > GridWorld.goalY) GridWorldAction.get("North")
      else if (y < GridWorld.goalY) GridWorldAction.get("South")
      else GridWorldAction.get("North")
    }
  }

}
