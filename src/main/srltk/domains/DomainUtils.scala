/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.domains
import srltk.common._
import scala.collection.mutable.ArrayBuffer

object DomainUtils
{  
  def generateSamples(n : Int, domain : SimDomain) : ArrayBuffer[(Int,Feats)] = {
    var samples = new ArrayBuffer[(Int,Feats)]
    for(i <- 0 until n){
      val a = GlobalRNG.nextInt(domain.domainDescription.numActions)
      val o = domain.act(IntAction(a))
      samples.append((a,asVectorCol(o.vec)))
    }
    samples
  }  
}
