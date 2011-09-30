package srltk.projects.psrs

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import org.scalatest.Tag
import scala.collection.mutable._
import org.scalatest.matchers.ShouldMatchers._
import srltk.projects.psrs.pomdps._
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class ACAutomatonTest extends FeatureSpec {


  //======================================================================
  feature("AC counting mode") {
    val rand = new util.Random()
    val numSeqs = 15
    val numSamples = 20
    val maxSeqLen = 6
    val max = 2
    val seqs = (for(i <- 0 until numSeqs) yield 
      (
        (for(j <- 0 until (rand.nextInt(maxSeqLen-1)+1))
         yield rand.nextInt(max)).toArray
      )).toArray
    val samples = for(i <- 0 until numSamples) yield rand.nextInt(max)
    println(samples)
    val autom = new ACAutomaton(seqs,max)
//    autom.display()
    
                                                
  }

}
