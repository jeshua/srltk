package srltk.projects.psrs
import scalala.tensor.dense._
import scalala.library.LinearAlgebra.svd
import scalala.library.LinearAlgebra.pinv
import scala.util.Random
import srltk.projects.psrs.pomdps._
import collection.mutable._



object DiscretePSRUtils{  

  def histsOfLength(n : Int, na : Int, no : Int) : Array[Array[(Int,Int)]] = {
    if(n <= 1) {
      val ret = new Array[Array[(Int,Int)]](na*no)
      for(a <- 0 until na;
          o <- 0 until no)
        ret(a * no + o) = Array((a,o))
      ret
    }
    else{
      val nm1 = histsOfLength(n-1,na,no)
      var ret = new ArrayBuffer[Array[(Int,Int)]]
      for(a <- 0 until na;
          o <- 0 until no)
        ret.appendAll((for(h <- nm1)
                      yield (a,o) +: h))
      ret.toArray
    }      
  }


  def expandHistories(hists : Array[Array[(Int,Int)]], na : Int, no : Int) : Array[Array[(Int,Int)]] = {
    var ret = new ArrayBuffer[Array[(Int,Int)]]
    //for every history in the list...
    for(h <- hists){
      //yield each possible expansion of h
      ret = ret ++(
        //returns Array[(Int,Int)]:
        for(a <- 0 until na;
            o <- 0 until no)
        yield h :+ (a,o))
    }
    ret.toArray
  }
  def extendTests(tests : Array[Array[(Int,Int)]], na : Int, no : Int) : Array[Array[(Int,Int)]] = {
    var ret  = new ArrayBuffer[Array[(Int,Int)]]
    //for every test in the list...
    for(t <- tests){
      //yield each possible expansion of h
      ret.appendAll(
        //returns Array[(Int,Int)]:
        for(a <- 0 until na;
            o <- 0 until no)
        yield (a,o) +: t)
    }
    ret.toArray
  }

  def generateTrajectory(length : Int, sim : POMDPSimulator) = {
    val rng = new Random()    
    //generate long sample trajectory
    var samples = new ArrayBuffer[(Int,Int)]
    for(i <- 0 until length){
      val oldState = sim.currentState
      val a = rng.nextInt(sim.numActions)
      val o = sim.act(a)
      samples.append((a,o))
    }
    samples.toArray
  }

}
