package srltk.projects.tdnets

import scalala.tensor.sparse.SparseVector
import org.scalatest.FeatureSpec
import srltk.tools.features.CMAC
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import srltk.projects._
@RunWith(classOf[JUnitRunner])
class MultiFeatureTransformTests extends FeatureSpec {   
  
  feature("testing stack transform - should stack input vectors into one resulting vector") {
    val v1 =  DenseVector[Double](1,2,3)
    val v2 =  DenseVector[Double](4,5)
    val stacked = (new StackTransform)(List(v1,v2).toArray)
    scenario("output size is the same"){
    	assert(stacked.length == 5)
    }
    scenario("output is correct"){
    	for(i <- 1 to 5)
    		assert(stacked(i-1) == i)
    }
  }
  
  feature("testing multi transform - should apply single feature extractor to each input and stack the results") {
    val v1 =  DenseVector[Double](1,2)
    val v2 =  DenseVector[Double](4,5)
    val ex = new CMAC(
        List((0, 3),
          (0, 5)),
        List(10, 10), 10, new scala.util.Random)
    val stacked = (new MultiTransform(ex))(List(v1,v2).toArray)
    scenario("output size is correct"){
    		assert(stacked.length == 10*10*10*2)
    }
    scenario("output sparsity is correct"){
    	assert(stacked.isInstanceOf[SparseVector[_]])
    }
  }
  
  
  
}