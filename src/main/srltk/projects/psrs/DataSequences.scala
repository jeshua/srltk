package srltk.projects.psrs
import srltk.projects._
import scala.collection.mutable.ArraySeq
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scala.collection.mutable.ArrayBuffer
import scalala.tensor.dense.DenseMatrix

package object DataSequences {
  type DataSequence = IndexedSeq[(Int, Feats)]

  //groups sequences with sliding window of size groupSize
  def groupSequences(longSeq: DataSequence, groupSize: Int): IndexedSeq[DataSequence] = {
    val ret = new ArrayBuffer[DataSequence]
    for (i <- 0 until longSeq.length - groupSize) {
      val subSeq = new ArrayBuffer[(Int, Feats)](groupSize)
      for (j <- i until i + groupSize)
        subSeq(j - i) = longSeq(j)
      ret.append(subSeq)
    }
    ret
  }

  //choose random sequences of length len out of data
  def chooseRandomSeqs(data: DataSequence, len: Int, num: Int, rng: util.Random): IndexedSeq[DataSequence] = {
    val indices =
      (for (i <- 0 until (data.length - len) / len) yield i * len)
    //shuffle the indices
    val m = rng.shuffle(indices)
    //return the first num
    for (i <- 0 until num) yield for (j <- 0 until len) yield data(m(i) + j)
  }

  
  //create matrix of sequence datapoints where each point is the concatenation of observations within sequence
  //columns are instances, rows are features
  def chooseRandomSeqObs(data: DataSequence, len: Int, num: Int, rng: util.Random): DenseMatrix[Double] = {
    val m = chooseRandomSeqs(data, len, num, rng)
    val featSize = data(0)._2.length * len //concatenated feature length 
    val matrix = DenseMatrix.zeros[Double](featSize,num) //feats are rows, instances are columns  
        
    for(sequenceI <- 0 until m.length){
      //loop through each element of sequence
      for(obsI <- 0 until m(sequenceI).length){
        val feat = m(sequenceI)(obsI)._2 //choose feature
        val start = obsI * feat.length //index of observation in final column
        matrix(start until start + feat.length, sequenceI) := feat
      }      
    }
    matrix
  }
  //===============================================================================
  //Sequence feature extractors

  trait SequenceFeatureExtractor {
    //extract feature vector for single sequence
    def extract(sequence: DataSequence): Feats
    //extract a collection of sequences
    def extract(sequences: Iterable[DataSequence]): Iterable[Feats] = {
      for (s <- sequences) yield extract(s)
    }
  }
  //Creates feature sequence by stacking observations into single column offset by action sequence	
  class SequenceIdentityExtractor(numActions: Int, sparse: Boolean = false) extends SequenceFeatureExtractor {

    def extract(sequence: DataSequence): Feats = {
      val featLen = sequence(0)._2.length
      val actionLen = sequence.length * featLen
      val totalLen = sequence.length * featLen * numActions
      val ret =
        if (sparse)
          SparseVector.zeros[Double](totalLen)
        else
          DenseVector.zeros[Double](totalLen)

      val actionIndex = (for (s <- sequence) yield s._1).foldLeft(0)(_ + _)

      for (i <- 0 until sequence.length) {
        val start = actionIndex * actionLen + i * featLen
        val end = start + featLen
        ret(start until end) := sequence(i)._2
      }
      ret
    }
  }
}