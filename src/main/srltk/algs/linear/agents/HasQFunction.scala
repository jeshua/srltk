package srltk.algs.linear.agents
import srltk.common._

trait HasQFunction {
  def getQ(o: Feats, a: Int): Double	
  def getMaxA(o: Feats): Int
}
