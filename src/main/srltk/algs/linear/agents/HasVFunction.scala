/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.algs.linear.agents
import srltk.common._

trait HasVFunction {
  def getV(o: Feats): Double	
}
