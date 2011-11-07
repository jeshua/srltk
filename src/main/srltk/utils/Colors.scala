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
package srltk.utils
import java.awt.Color

object Colors {

  private val colors = List(new Color(255, 0, 0), new Color(0, 255, 0), new Color(0, 0, 255),
    new Color(255, 255, 0), new Color(0, 255, 255), new Color(255, 0, 255),
    new Color(150, 0, 0), new Color(0, 150, 0), new Color(0, 0, 150),
    new Color(150, 150, 0), new Color(0, 150, 150), new Color(150, 0, 150),
    new Color(75, 0, 0), new Color(0, 75, 0), new Color(0, 0, 75),
    new Color(75, 75, 0), new Color(0, 75, 75), new Color(75, 0, 75));

  def colorProgression(i: Int) = colors(i % colors.length)

}
