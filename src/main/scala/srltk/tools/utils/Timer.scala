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
 ***************************************************************************** */
package srltk.tools.utils
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

package object Timer {

  //returns time in milliseconds
  def time(f : () => Unit) : Double = {
      val start = userTime()
      f()
      val end = userTime()
      (end-start)/1000000d
  }
  
  def timeCpu(f : () => Unit) : Double = {
      val start = cpuTime()
      f()
      val end = cpuTime()
      (end-start)/1000000d
  }
  
   def timeWallclock(f : () => Unit) : Double = {
      val start = System.nanoTime()
      f()
      val end = System.nanoTime()
      (end-start)/1000000d
  }
  def userTime() : Long = {
      val bean = ManagementFactory.getThreadMXBean( );
      return bean.getCurrentThreadUserTime()          
    }
   def cpuTime() : Long = {
      val bean = ManagementFactory.getThreadMXBean( );
      return bean.getCurrentThreadCpuTime()          
    }
}
