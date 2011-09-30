/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
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
 ******************************************************************************/
package srltk.test.domains
import srltk.api.domain._
import GridWorld._

object GridWorldTests {

  //just takes shortest path to the goal, with some randomness
  class Policy1(gamma: Double)
    extends TestPolicy {

    def V2D(x: Int, y: Int) = getValue(new GridWorldState(x, y))

    def getValue(s: State) = {
      val state = s.asInstanceOf[GridWorldState]
      val distance = scala.math.abs(state.x - GridWorld.goalX) + scala.math.abs(state.y - GridWorld.goalY) - 1
      //straight-line distance to goal
      if (distance <= 0) 0
      else if (gamma == 1) -distance
      else -1 * ((1 - scala.math.pow(gamma, distance)) / (1 - gamma))
    }

    def getInitialState() = GridWorld.getInitial

    def getAction(s: State) = {
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
