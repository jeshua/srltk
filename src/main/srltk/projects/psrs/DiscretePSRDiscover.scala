package srltk.projects.psrs
import scalala.tensor.dense._
import scalala.library.LinearAlgebra.svd
import scalala.library.LinearAlgebra.pinv
import scala.util.Random
import srltk.projects.psrs.pomdps._
import collection.mutable._



class DiscretePSRDiscover(val sim : POMDPSimulator) {

  /*val na = sim.numActions
  val no = sim.numObservations

  def discover() {
    import DiscretePSRUtils._
    val numSamples = 20000
    val samples = generateTrajectory(numSamples,sim)

    var H : Array[Array[(Int,Int)]] = Nil
    var Q : Array[Array[(Int,Int)]] = Nil
    val empty : Array[Array[(Int,Int)]] = Array(Nil)
    var Hl1 = histsOfLength(1,na,no)
    var Tl1 = histsOfLength(1,na,no)

    val CH = empty ++ Hl1 ++ H ++ expandHistories(H,na,no)
    val CQ = Tl1 ++ Q ++ extendTests(Q,na,no)

    //construct D
    val db = new SampleDatabase(na,no,CH,CQ)
    db.append(samples)
    db.updateCounts()
    val D = DenseMatrix.zeros[Double](CH.length,CQ.length)
    for(i <- 0 until CH.length;
        j <- 0 until CQ.length)
      D(i,j) = db.getConditionalProb(i,j)

    //find linearly independent rows/cols
    val s = svd(D)
    

  }

*/
}
