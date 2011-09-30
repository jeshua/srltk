package srltk.projects.psrs
import scala.collection.mutable._
import util.control.Breaks._
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import srltk.projects.psrs.pomdps._
import util.Random
import scala.math._
import scala.util.Sorting._
import scala.collection.mutable._

object Discover{
  type Sequence = List[(Int,Int)]
  
  def apply(maxSeqLen : Int, 
            maxPerLen : Int,
            samples : ArrayBuffer[(Int,Int)],
            random : Random,
            exclusive : Boolean = false
          ) 
  : Array[Array[(Int,Int)]]
  = {
    var seqs = new ArrayBuffer[Array[(Int,Int)]]
    
    val start = if(exclusive) maxSeqLen else 1
    //loop through sequence lengths
    for(size <- start to maxSeqLen) {
      val tempSeqMap = new HashMap[Sequence,Long]
      //loop through sequences of this length
      for(i <- 0 until (samples.length-size)) {
        val key = (for(m <- i until i+size) yield(samples(m))).toList
        if(!tempSeqMap.contains(key))
          tempSeqMap(key) = 1
        else
          tempSeqMap(key) += 1
      }//end loop over samples

      //pick the mist frequent tests
      val maxSeqs = maxPerLen

      var tempSeqs = new ArrayBuffer[(Long,Sequence)]
      for((k,v) <- tempSeqMap) tempSeqs.append((v,k))
      val sorted = stableSort(tempSeqs,
                              (a : (Long, Sequence), b : (Long, Sequence)) => a._1 > b._1)
      val num = min(maxSeqs,sorted.length)
      //printf("choosing %d seqs of length %d\n",num,size)
      for(i <- 0 until num)
        seqs.append(sorted(i)._2.toArray)
    }//end loop over size
    //println("In total there are "+seqs.length+" seqs\n")
    seqs.toArray
  }
  
  
  def main(args : Array[String]){
    val pomdp = new FloatReset()
    val numSamples = 50000
    val rng = new Random()
    val samples = pomdp.generateSamples(numSamples,rng)
    Discover(8,10,samples,rng)
  }
}
