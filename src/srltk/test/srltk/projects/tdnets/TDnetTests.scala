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
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Test
import org.junit.Assert._
import srltk.projects._
@RunWith(classOf[JUnitRunner])
class TDnetTests extends FunSuite with ShouldMatchersForJUnit {
  test("simple net with one one node") {
    val alpha = 0.01
    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim,true)
    val t1 = new InternalNodePrototype("Node1")
    t1 predicts LeafNodes.ObservationNode
    t1 observes LeafNodes.ObservationNode withAnswerPredictor td
    withClue("output dimension inferred correctly")
    {assert(t1.inferOutputDimension(1) === 1)}
    
    val net = new TDNet(t1,1)
    val node1 = net.internalNodes("Node1")
        
    withClue("yDim is right")
    {assert(node1.yDim === 1)}
    withClue("xDim is right")
    {assert(node1.xDim === 1)}
    withClue("yT is right")
    {assert(node1.state.yT.length === 1)}
    withClue("predictor has correct input dimension")
    {assert(node1.answerPredictor.iDim === 1)}
    withClue("predictor has correct output dimension")
    {assert(node1.answerPredictor.oDim === 1)}
    val f = new Observation(toFeats(1))
    val newState =  node1.observe(f,null)
    withClue("xT is right")
    {assert(newState.xT.length === 1)}
    withClue("yT is right")
    {assert(newState.yT.length === 1)}
    withClue("yTp1 is right")
    {assert(newState.yTp1.length === 1)}
  
    
    val predictor = new MultivariateTD(alpha,0,1,1,true)

    val rng = new scala.util.Random(5)
    val p = .4
    var last_l = 0
    node1.state = 
    	new NodeState(0,toFeats(last_l),toFeats(0),null,null,node1.state.yTp1,node1.questionPredictor,node1.answerPredictor)
    
    node1.state.yT(0)
    val thresh = 0.000001
    for(i <- 0 until 2){
    	//generate new observation
    	val l = if(rng.nextDouble() < p) 1 else 0
    	
    	
    	assertEquals("td net's last input matches "+i,
    			last_l,node1.state.xT(0),thresh)
    	
    	//get their predictions of this observation
    	val pr1 : Double = node1.state.yT(0)
    	val pr2 : Double = predictor.predict(toFeats(last_l))(0)
    	assertEquals("td net prediction is the same as plain td predictor at iter "+i,
    			pr2,pr1,thresh)
    	
    	//learn
    	net.learn(l)
    	val er1 = net.lastErrors("Node1")
    	val er2 = predictor.learn(toFeats(last_l),toFeats(l - pr2))
    	assertEquals("td net error the same as plain td predictor error at iter "+i,
    			er2,er1,thresh)
    
    	//modify their learning rate
    	node1.answerPredictor.asInstanceOf[MultivariateTD].alpha *= .999
    	predictor.alpha *= .999
    	last_l = l
    	
    	assertEquals("td net weights(0,0) are the same as plain td weights at iter "+i,
    			predictor.theta(0,0),node1.answerPredictor.asInstanceOf[MultivariateTD].theta(0,0),thresh)
    	assertEquals("td net weights(0,1) are the same as plain td weights at iter "+i,
    			predictor.theta(0,1),node1.answerPredictor.asInstanceOf[MultivariateTD].theta(0,1),thresh)
    	
    }
  }
  /*
   * 
   */
  test("create net with disjoint nodes") {
    val alpha = 0.01
    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim,true)
    val t1 = new InternalNodePrototype("Node1")
    val t2 = new InternalNodePrototype("Node2")
    val t3 = new InternalNodePrototype("Node3")
    t1 predicts LeafNodes.ObservationNode
    t1 observes LeafNodes.ObservationNode withAnswerPredictor td
    t2 predicts t3 withAnswerPredictor td
    t3 predicts LeafNodes.ObservationNode withAnswerPredictor td
    val net = new TDNet(List(t1,t2,t3),1)
    assertEquals("net has correct number of nodes",
    		4,net.allNodes.size)    
    
  }
}
