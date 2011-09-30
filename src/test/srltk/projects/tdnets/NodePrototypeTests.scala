package srltk.projects.tdnets

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import srltk.projects._
@RunWith(classOf[JUnitRunner])
class InternalNodePrototypeTests extends FeatureSpec {

  import LeafNodes._

  //======================================================================

  feature("constructing self-referential networks") {
    val node1 = new InternalNodePrototype("Node1")
    scenario("node does not specify its size, so an exception should be thrown") {
      try {
        node1.predicts(node1)
        fail()
      } catch { case _ => }
    }

    scenario("exception should also be thrown if self reference is in a list of predictions") {
      try {
        node1.predicts(List(node1, LeafNodes.ObservationNode, LeafNodes.RewardNode))
        fail()
      } catch { case _ => }
    }

    val node2 = new InternalNodePrototype("Node2", (a: Int) => 1)
    scenario("node does specify its size, so no exception should be thrown") {
      try {
        node2.predicts(node2)
      } catch { case _: IllegalArgumentException => fail(); }
    }
  }

  //======================================================================

  feature("infer node's output dimension") {

    
    scenario("one node network, one bit observation") {
      val node1 = new InternalNodePrototype("Node1")
      node1 predicts ObservationNode
      assert(node1.inferOutputDimension(1) === 1)
    }
    
    scenario("simple network without user-defined target function") {
      val obsDim = 8
      val node1 = new InternalNodePrototype("Node1")
      val node2 = new InternalNodePrototype("Node2")
      val node3 = new InternalNodePrototype("Node3")
      node1 predicts List(node2, node3) //size 25
      node2 predicts ObservationNode //size 8
      node3 predicts List(node2, RewardNode, ObservationNode) // size 17
      assert(node1.inferOutputDimension(obsDim) === 25)
    }
    

    scenario("self referential network should determine size using the size specified") { 
      val obsDim = 8
      val node4 = new InternalNodePrototype("Node4", (a: Int) => 4)
      node4 predicts node4 // 1+8+4
      assert(node4.inferOutputDimension(obsDim) === 4)
    }
    scenario("user defined target functions"){
      val node1 = new InternalNodePrototype("Node1")
      val node2 = new InternalNodePrototype("Node2")
      val obsDim = 8
      def func(f : Array[Feats]) : Feats = {f(0)(List(0,1,2)).asCol}
      node1 predicts(ObservationNode,func _)
      node1 observes ObservationNode
      node2 predicts node1
      node2 observes ObservationNode
      
      assert(node1.inferOutputDimension(obsDim) === 3)
      assert(node2.inferOutputDimension(obsDim) === 3)
      
    }
  }

  //======================================================================

  feature("infer node's input dimension") {
    import LeafNodes._
    val obsDim = 8
    val node1 = new InternalNodePrototype("Node1")
    val node2 = new InternalNodePrototype("Node2")
    val node3 = new InternalNodePrototype("Node3")
    scenario("simple network without user-defined input function") {
      node1 observes List(node2, node3) //size 25
      node2 predicts ObservationNode //size 8
      node3 predicts List(node2, RewardNode, ObservationNode) // size 17
      assert(node1.inferInputDimension(obsDim) === 25)
    }

    scenario("user defined input functions")(pending)
  }

  //======================================================================

  feature("generate nodes from a node prototype") {
    val node1 = new InternalNodePrototype("Node1")
    val node2 = new InternalNodePrototype("Node2")
    val node3 = new InternalNodePrototype("Node3")
    node1 predicts List(node2, node3) //size 25
    node2 predicts ObservationNode //size 8
    node3 predicts List(node2, RewardNode, ObservationNode) // size 17
    
    
    scenario("network requires an answer regressor, it should fail if we don't specity one") {
      try {
	node1.generate(10,5)
	fail()
      } catch { case _: IllegalArgumentException => }
    }
    
    def td = (inDim: Int, outDim: Int) => new MultivariateTD(.1, 0, inDim, outDim)
    scenario("generate succeeds and resulting network has the correct number of nodes") {
      node1.answerRegressor = td
      node2.answerRegressor = td
      node3.answerRegressor = td
      try {
	val n1 = node1.generate(10,5)
        val flat = n1.flatten()
 	assert(flat.length===5)
      } catch { case _: IllegalArgumentException => fail()}
    }
  }

}
