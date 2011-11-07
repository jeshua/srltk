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
package srltk.test.control

import srltk.domains.rlstandard._
import srltk.algs.linear.agents.EGreedySarsa

object TestSarsaGridWorld {
  import GridWorld._

  def main(args: Array[String]): Unit = {

    val rng = new scala.util.Random()
    //create mountain car
    val domain = new GridWorld()
    val agent = new EGreedySarsa(GridWorld.dd,.01, .01, 0, .99, 0)
    TestRun(domain, agent,Some(1000),None);
  }
}
