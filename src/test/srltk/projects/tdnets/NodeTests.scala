package srltk.projects.tdnets

import org.scalatest.matchers.MustMatchers
import org.scalatest.GivenWhenThen
import scala.reflect.Method
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import org.scalatest.FeatureSpec
import srltk.api.domain._
import scala.collection.mutable.HashMap
import srltk.projects._
@RunWith(classOf[JUnitRunner])
class NodeTests extends FeatureSpec with MustMatchers {
	 //======================================================================

	  feature("node update timing") {

  import LeafNodes._
  val t1 = new InternalNodePrototype("Node1")
  val t2 = new InternalNodePrototype("Node2")
  t1 observes ObservationNode
  t1 predicts List(ObservationNode, t2)
  t2 predicts ObservationNode
  t2 observes List(ObservationNode, t1)

  class FakePredictor(inDim: Int, outDim: Int) extends Predictor(inDim, outDim) {
    def predict(input: Feats) =
      {
        val v = DenseVector.zeros[Double](outDim)
        for (i <- 0 until outDim) v(i) = if (i < input.length) input(i) else -1
        v
      }
    def learn(input: Feats, error: Feats) = 0
  }

  def td = (inDim: Int, outDim: Int) => new FakePredictor(inDim, outDim)
  t1.answerRegressor = td
  t2.answerRegressor = td

  val obs1 = new Observation(DenseVector[Double](1, 2, 3))

  //create the network
  val net = t1.generate(3, 1)
  val nodes = net.flatten()
  def getNodeByName(name: String): Option[Node] = {
    nodes.find(_.name.equals(name))
  }
  val node1: InternalNode = getNodeByName("Node1").get.asInstanceOf[InternalNode]
  val node2: InternalNode = getNodeByName("Node2").get.asInstanceOf[InternalNode]
  
 
    scenario("a simple network is initialized") {
      withClue("node1 yDim is right") { node1.yDim must be === obs1.features.length * 2 }
      withClue("node1 xDim is right") { node1.xDim must be === obs1.features.length }

      withClue("node2 yDim is right") { node2.yDim must be === obs1.features.length }
      withClue("node2 xDim is right") { node2.xDim must be === obs1.features.length * 3 }

      withClue("the timestep should be 0 in all nodes") {
        node1.state.timestep must be === 0
        node2.state.timestep must be === 0
      }

      withClue("node1's xT should be the right length") { node1.state.xT.length must be === obs1.features.length }

      withClue("node2's xT should be the right length") { node2.state.xT.length must be === obs1.features.length * 3 }

      withClue("node1's yT should be the right length") { node1.state.yT.length must be === obs1.features.length * 2 }

      withClue("node2's yT should be the right length") { node2.state.yT.length must be === obs1.features.length }

    }

    scenario("after running a network for one step") {
      val newStates = new HashMap[String, NodeState]
      for (n <- nodes) 
    	  n match{ case t : InternalNode => newStates(n.name) = t.observe(obs1, new Action(0, 1)); case _ => ()}
      
      withClue("the timestep should be incremented") { newStates("Node1").timestep must be === 1 }
      withClue("node 1 x is of correct size") { newStates("Node1").xTp1.length must be === node1.xDim }
      withClue("node 1 x has correct values") { newStates("Node1").xTp1(2) must be === obs1.features(2) }
      withClue("node 1 ytp1 is of correct size") { newStates("Node1").yTp1.length must be === node1.yDim }
      withClue("node 1 ytp1 contains correct value") { newStates("Node1").yTp1(2) must be === newStates("Node1").xTp1(2) }
      withClue("node 1 K should be the right length") { node1.K(obs1).length must be === obs1.features.length + node2.state.yTp1.length }

      withClue("node 2 xT should contain observation") { newStates("Node2").xTp1.length must be === obs1.features.length + node1.state.yTp1.length }
      withClue("node 2 x should contain observation") { newStates("Node2").xTp1(1) must be === 2 } //should be 2 because obs1's value    	  
      withClue("node 2 x should contain node1's prediction ") { newStates("Node2").xTp1(5) must be === 0 } //should be 0 because node1 should predict 0's on the first timestep    	  
      withClue("node 2 y t+1 should be yDim") { newStates("Node2").yTp1.length must be === obs1.features.length }
      withClue("node 2 y t+1 should be prediction should be created from xtp") { newStates("Node2").yTp1(1) must be === 2 }
      withClue("node 2 K should be the right length") { node2.K(obs1).length must be === obs1.features.length }
    }

    scenario("after updating network once") {
    	node1.state = node1.observe(obs1, new Action(0, 1))
    	node2.state = node2.observe(obs1, new Action(0, 1))
    	node1.update
    	node2.update
      var k = node1.K(obs1)

      withClue("target nodes") { node1.targetNodes.length must be === 2 }
      withClue("K size should have size ydim") { k.length must be === node1.yDim }
      withClue("node1 ydim is right size") { node1.yDim must be === 6 }
      withClue("K size should have size equal to stacked targets") { k.length must be === node2.yDim + obs1.features.length }
      withClue("node 1 K should be obs node with t1.yTp1 underneath") {
        k(0) must be === obs1.features(0)
        k(1) must be === obs1.features(1)
        k(2) must be === obs1.features(2)
        k(3) must be === node2.state.yTp1(0)
        k(4) must be === node2.state.yTp1(1)
        k(5) must be === node2.state.yTp1(2)
      }
      withClue("node 1 Z should equal K because we didn't set target function") { assert(node1.Z(k) === k) }

      withClue("node 1 should have correct Z produced from K") { node1.Z(node1.K(obs1))(2) must be === k(2) }

      k = node2.K(obs1)
      withClue("k is ydim length") { k.length must be === node2.yDim }
      withClue("node2 ydim is right size") { node2.yDim must be === 3 }

      withClue("node 2 K size should have size equal to observation") { k.length must be === obs1.features.length }
      withClue("node 2 K should be obs") {
        k(0) must be === obs1.features(0)
        k(1) must be === obs1.features(1)
        k(2) must be === obs1.features(2)
      }
    }
  }
}
