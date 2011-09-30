package srltk.projects.psrs

import org.scalatest.FunSuite
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import scala.util.Random
import srltk.projects.psrs.pomdps.Tiger
import org.scalatest.matchers.MustMatchers
import org.scalatest.GivenWhenThen
import scala.reflect.Method
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.dense.DenseVector
import srltk.api.domain._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Test
import org.junit.Assert._
import org.scalatest.matchers.ShouldMatchers._

@RunWith(classOf[JUnitRunner])
class DiscretePSRDiscoverTest extends FunSuite with ShouldMatchersForJUnit {

  //======================================================================
  test("discete psr utils work") {
    import DiscretePSRUtils._

    withClue("generate histories of length 2")
    {
      val hists = histsOfLength(2,1,2)
      hists(0) should equal (Array((0,0),(0,0)))
      hists(1) should equal (Array((0,0),(0,1)))
      hists(2) should equal (Array((0,1),(0,0)))
      hists(3) should equal (Array((0,1),(0,1)))
    }

    withClue("expand histories")
    {
      val hists = expandHistories(Array(Array((0,0))),1,3)
      hists(0) should equal (Array((0,0),(0,0)))
      hists(1) should equal (Array((0,0),(0,1)))
      hists(2) should equal (Array((0,0),(0,2)))
    }

    withClue("extend tests")
    {
      val tests = extendTests(Array(Array((0,0))),1,3)
      tests(0) should equal (Array((0,0),(0,0)))
      tests(1) should equal (Array((0,1),(0,0)))
      tests(2) should equal (Array((0,2),(0,0)))
    }
  }
}
