package srltk.projects.psrs

import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import scala.util.Random
import org.junit.Test
import org.junit.Assert._
import srltk.projects.psrs.pomdps._
import scalala.tensor.dense._
import scalala.operators._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class DiscreteTPSRTest extends FeatureSpec {

  //======================================================================

  def rep(v: DenseVectorCol[Double], n: Int): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](v.length, n)
    for (i <- 0 until n)
      ret(0 until v.length, i) := v
    ret
  }

/*   feature("tpsr learning tiger pomdp") {
   test(new Tiger)
   }
   feature("tpsr learning float reset pomdp") {
   test(new FloatReset)
   }
   feature("tpsr learning paint pomdp") {
   test(new Paint)
   }*/
  def test(pomdp: POMDPSimulator) = {
    val rank = 20
    val debug = false
    val na = pomdp.numActions
    val no = pomdp.numObservations

    //generate data from random policy
    val rng = new scala.util.Random(9)
    pomdp.random = rng
    var samples = new ArrayBuffer[(Int, Int)]

    val numSamples = 50000
    val stateSum = new Array[Int](pomdp.numStates)
    for (i <- 0 until numSamples) {
      val a = rng.nextInt(pomdp.numActions)
      stateSum(pomdp.currentState) += 1
      val o = pomdp.act(a)
      samples.append((a, o))
    }

    val tests = DiscretePSRUtils.histsOfLength(2, na, no)
    val hists = DiscretePSRUtils.histsOfLength(2, na, no)

    val db = new DiscreteSampleDatabaseAC(pomdp.numActions, pomdp.numObservations, hists, tests)
    db.append(samples)
    val b = pomdp.beliefSteadyState(pomdp.uniformPolicy)
    //now learn a psr
    val m = new DiscreteTPSR(db, .03, rank)
    val PT = m.computePT()
    val PTH = m.computePTH()
    val PH = m.computePH()
    val bPH = pomdp.beliefPH(b, db.histories)
    val jointPTH = PTH :* rep(PT, PTH.numCols)
    val bjointPTH = pomdp.beliefJointPTH(b, db.tests, db.histories)
    val thresh = 0.05
    //make sure steady state is right
    scenario("POMDP steady state is right") {
      for (s <- 0 until pomdp.numStates)
        assertEquals(stateSum(s) / numSamples.toDouble, b(s), thresh)
    }

    //mutually exclusive histories so PH should be a probability distribution
    scenario("PH is a probability vector") {
      assertEquals(PH.sum, 1, thresh)
    }
    scenario("PTH is a joint-probability matrix (tests and histories are mutually exclusive and exhaustive in this unit test)") { assertEquals(1, m.jointPTHSum, thresh) }

    scenario("PTaoH is a joint-probability tensor (tests and histories are mutually exclusive and exhaustive in this unit test)") {
      assertEquals(1, m.jointPTaoHSum, thresh)
    }

    scenario("p(ht) matches that computed by pomdp") {
      for (t <- 0 until tests.length)
        for (h <- 0 until hists.length)
          assertEquals(bjointPTH(t, h), jointPTH(t, h), thresh)
    }

    scenario("p(t|h) matches that computed by pomdp") {
      for (t <- 0 until tests.length)
        for (h <- 0 until hists.length) {
          if (bPH(h) > 0 && PH(h) > 0)
            assertEquals(bjointPTH(t, h) / bPH(h), jointPTH(t, h) / PH(h), thresh)
        }
    }

    scenario("p(h,t|t^a) matches that computed by pomdp") {
      for (t <- 0 until tests.length)
        for (h <- 0 until hists.length)
          assertEquals(bjointPTH(t, h) / PT(t), PTH(t, h), thresh)
    }

    if (debug) {
      println("true pr(ht)")
      for (t <- 0 until tests.length) {
        for (h <- 0 until hists.length)
          printf("%.4f\t", bjointPTH(t, h))
        println()
      }
      println("est pr(ht) =")
      for (t <- 0 until db.tests.length) {
        for (h <- 0 until db.histories.length) {
          printf("%.4f\t", (jointPTH(t, h)))
        }
        println()
      }
      println()
      println("true pr(ht^o|t^a)")
      for (t <- 0 until tests.length) {
        for (h <- 0 until hists.length)
          printf("%.4f\t", bjointPTH(t, h) / PT(t))
        println()
      }
      println("est pr(ht^o|t^a)=")
      for (t <- 0 until db.tests.length) {
        for (h <- 0 until db.histories.length)
          printf("%.4f\t", PTH(t, h))
        println()
      }
      println()
      println("true pr(t|h)")
      for (t <- 0 until tests.length) {
        for (h <- 0 until hists.length)
          printf("%.4f\t", bjointPTH(t, h) / PH(h))
        println()
      }
      println("est pr(t|h) =")
      for (t <- 0 until db.tests.length) {
        for (h <- 0 until db.histories.length)
          printf("%.4f\t", (PTH(t, h) * PT(t) / PH(h)))
        println()
      }
    }

    scenario("MSE within tolerance") {
      try {
        m.learn()
      } catch {
        case e: Exception => e.printStackTrace()
      }
      val mse = (new EvaluateDiscreteTPSR(m, pomdp)).testErr
      assert(mse < 0.001)
    }

  }

  //==================================================
  feature("core tests") {
    testCoreTests(new Chain(4))
  }

  def testCoreTests(pomdp: POMDPSimulator) = {
    val maxrank = 20
    val debug = false
    val na = pomdp.numActions
    val no = pomdp.numObservations

    //generate data from random policy
    val rng = new scala.util.Random(9)
    pomdp.random = rng
    val numSamples = 500000
    var samples = pomdp.generateSamples(numSamples,rng)
    val t = Discover(7,65,samples,rng)  
    val tests = t
    val hists = Discover(7,300,samples,rng,true)  
    printf("There are %d tests and %d hists\n",t.length,hists.length)
    val db = new DiscreteSampleDatabaseAC(pomdp.numActions, pomdp.numObservations, hists, tests)
    db.append(samples)
    val m = new DiscreteTPSR(db, .01, maxrank)

    
    import srltk.tools.linearalgebra.LinearAlgebra._
    val pth = m.computePTgivenH()
    val rank = estRank(pth,0.1)    
    println("rank is "+rank)


    val qr1 = qrp(pth)
    val qr2 = qrp(transpose(pth))
    println(qr1._4)
    println(qr2._4)
  }

}
