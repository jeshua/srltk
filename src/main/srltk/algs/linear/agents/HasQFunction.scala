/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.algs.linear.agents
import srltk.common._

trait HasQFunction {
  def getQ(o: Feats, a: Int): Double	
  def getMaxA(o: Feats): Int
}
