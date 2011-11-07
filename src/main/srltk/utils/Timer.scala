package srltk.utils
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


  def timeWallclock[V](f : () => V) : (Double,V) = {
      val start = System.nanoTime()
      val ret = f()
      val end = System.nanoTime()
      ((end-start)/1000000d,ret)
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
