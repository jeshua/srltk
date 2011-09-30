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
class FixedHistoryArrayTest extends FeatureSpec {


  //======================================================================
  feature("AC counting mode") {
    val pomdp = new Tiger
    val na = pomdp.numActions
    val no = pomdp.numObservations
    val numSamples = 60000
    val rng = new scala.util.Random()
    val samples = pomdp.generateSamples(numSamples,rng)
    val t = Discover(4,3,samples,rng)
    val db1 = new DiscreteSampleDatabaseNaive(pomdp.numActions,
                                        pomdp.numObservations,
                                        t,t)
    val db2 = new DiscreteSampleDatabaseAC(pomdp.numActions,
                                           pomdp.numObservations, 
                                           t,t)
    db1.append(samples)
    db2.append(samples)
    
    scenario("hcounts match",Tag("hola")){    
      for(i <- 0 until db1.hCount.length)
        assertEquals(db1.hCount(i),db2.hCount(i))
    }    
    scenario("tcounts match"){    
      for(i <- 0 until db1.tCount.length)
        withClue("For test "+i+" = "+db1.tests(i).toList){
          assert(db1.tCount(i) === db2.tCount(i))
        }
    }    
  }

  //======================================================================

  feature("counting histories and tests") {
    val histories = Array(
      Array((0,3),(1,3)),
      Array((0,2), (2, 3)),
      Array((1,2), (1, 2)),
      Array((3,4)),
      Array((4,0)))
    val tests = Array(
      Array((1,2)),
      Array((0, 1), (3, 2)),
      Array((0,2),(1,2),(1,4),(0,3)))
    val db = new DiscreteSampleDatabaseAC(5, 5, histories, tests)
    db.append(Array(
      (0,3),
      (1,3),
      (2,1),
      (2,1),
      (2,1),
      (0,3),
      (4,0),
      (0,1),
      (1,2),
      (1,2),
      (0,2),
      (2,3),
      (0,1),
      (3,2),
      (1,2),
      (1,2),
      (1,2)))
    scenario("correct h count"){    
      withClue("history 0 count is correct")
      {assert(db.hCount(0) === 1)}    	
      withClue("history 1 count is correct")
      {assert(db.hCount(1) === 1)}    	
      withClue("history 2 count is correct")
      {assert(db.hCount(2) === 2)}    	
      withClue("history 3 count is correct")
      {assert(db.hCount(3) === 0)}    	
    }    
    
    scenario("test actions correct"){
      withClue("test action count")
      {assert(db.testActions.length === 3)}

      withClue("test actions contains (1)")
      {db.testActions(0) should equal (Array(1))}
      withClue("test actions contains (0,3)")
      {db.testActions(1) should equal (Array(0,3))}
      withClue("test actions contains (0,1,1,0)")
      {db.testActions(2) should equal (Array(0,1,1,0))}
    }

    scenario("correct test action count"){
      withClue("test 0 count is correct")
      {assert(db.getAttemptCount(0) === 6)}
      withClue("test 1 count is correct")
      {assert(db.getAttemptCount(1) === 1)}
      withClue("test 2 count is correct")
      {assert(db.getAttemptCount(2) === 1)}
    }
  }
}
