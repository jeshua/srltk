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
package srltk.tools.learners

import srltk.api.domain.Observation
import srltk.api.agent._
import srltk.api.domain.Action
import scala.util.Random

abstract class LearnerQ(val rng: Random) extends CanLearn with HasQFunction with Imprintable {

  def getQ(o: Observation, a: Action): Double

  def getMaxA(o: Observation): Int =
    {
      val vals = for (i <- 0 until numActions) yield getQ(o, i)
      val max = vals.reduceLeft(scala.math.max _)
      val maxs = for (i <- 0 until numActions; if vals(i) == max) yield i
      maxs(rng.nextInt(maxs.length))
      maxs(0)
    }
}
