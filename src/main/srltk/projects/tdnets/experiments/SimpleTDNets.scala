package srltk.projects.tdnets.experiments
import srltk.projects.tdnets._
import srltk.api.domain._
import srltk.api.agent._
import srltk.vis.ActivePlot

object SimpleTDNets {

  def main(args: Array[String]) {
    //test1()
    //test2()
    //test3()
	//test4()
    //layered()
    //layered2()
    multipass()
  }

  //======================================================================

  def test1() = {
    val alpha = .01
    val rng = new scala.util.Random(5)

    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim, true)
    val t1 = new InternalNodePrototype("Node1")
    t1 observes LeafNodes.ObservationNode withAnswerPredictor td
    t1 predicts LeafNodes.ObservationNode withAnswerPredictor td

    val net = new TDNet(t1, 1)
    net.showMonitor()
    val p = .7
    for (i <- 0 until 20000) {
      val l = if (rng.nextDouble() < p) 1 else 0
      net.learn(l)
    }
  }

  //======================================================================

  def test2() = {
    val alpha = .05
    val rng = new scala.util.Random(5)

    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim, true)
    val t1 = new InternalNodePrototype("Node1")
    val t2 = new InternalNodePrototype("Node2")

    t1 observes LeafNodes.ObservationNode withAnswerPredictor td observes t1 observes t2
    t1 predicts t2

    t2 observes LeafNodes.ObservationNode withAnswerPredictor td observes t1 observes t2
    t2 predicts LeafNodes.ObservationNode

    val net = new TDNet(t1, 1)
    net.showMonitor()
    val p = .7
    for (i <- 0 until 3000) {
      val l = if (rng.nextDouble() < p) 1 else 0
      net.learn(l)
    }
  }
  //======================================================================

  def test3(alternate: Boolean = false) = {
    val alpha1 = .001
    val alpha2 = .01
    val rng = new scala.util.Random(5)

    def td1 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha1, 0, inDim, outDim, true)
    def td2 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha2, 0, inDim, outDim, true)
    val t1 = new InternalNodePrototype("Node1")
    val t2 = new InternalNodePrototype("Node2")

    t1 observes LeafNodes.ObservationNode withAnswerPredictor td1 observes t1 observes t2
    t1 predicts t2

    t2 observes LeafNodes.ObservationNode withAnswerPredictor td2 observes t1 observes t2
    t2 predicts LeafNodes.ObservationNode

    val net = new TDNet(t1, 1)
    val steps = 50000
    net.showMonitor(steps/500)
    val node1: InternalNode = net.internalNodes("Node1")
    val node2: InternalNode = net.internalNodes("Node2")

    var last_l = 0
    var last_last_l = 0
    //for alternating
    var turn = 0
    val learnSteps = 10000 //if alternating    

    for (i <- 0 until steps) {

      if (alternate) {
        if (i % learnSteps == 0) {
          println(turn + " switching at " + i)
          turn = ((turn + 1) % 2)
        }

        if (turn == 0) {
          //alternating learning:
          node1.answerPredictor.asInstanceOf[MultivariateTD].alpha = alpha1
          node2.answerPredictor.asInstanceOf[MultivariateTD].alpha = 0
        } else if (turn == 1)
          node1.answerPredictor.asInstanceOf[MultivariateTD].alpha = 0
        node2.answerPredictor.asInstanceOf[MultivariateTD].alpha = alpha2
      }

      val p =
        (last_l, last_last_l) match {
          case (0, 0) => .99
          case (0, 1) => .01
          case (1, 0) => .99
          case (1, 1) => .01
        }
      val l = if (rng.nextDouble() < p) 1 else 0
      net.learn(l)
      last_last_l = last_l
      last_l = l

    }
  }
  //======================================================================

  def test4() = {
    val alpha1 = .01
    val alpha2 = .001
    val alpha3 = .0001
    val rng = new scala.util.Random(5)

    def td1 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha1, 0, inDim, outDim, true)
    def td2 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha2, 0, inDim, outDim, true)
    def td3 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha3, 0, inDim, outDim, true)
    val t1 = new InternalNodePrototype("Node1")
    val t2 = new InternalNodePrototype("Node2")
    val t3 = new InternalNodePrototype("Node3")

    t1 observes List(LeafNodes.ObservationNode,t1,t2,t3) predicts LeafNodes.ObservationNode withAnswerPredictor td1
    t2 observes List(LeafNodes.ObservationNode,t1,t2,t3) predicts t1 withAnswerPredictor td2
    t3 observes List(LeafNodes.ObservationNode,t1,t2,t3) predicts t2 withAnswerPredictor td3
    
    val net = new TDNet(List(t1,t2,t3), 1)
    val node1: InternalNode = net.internalNodes("Node1")
    val node2: InternalNode = net.internalNodes("Node2")
    val node3: InternalNode = net.internalNodes("Node3")

    val steps = 500000
    net.showMonitor(steps/500)
    var last_l = 0
    var last_last_l = 0

    for (i <- 0 until steps) {
    	val p =
        (last_l, last_last_l) match {
          case (0, 0) => .7
          case (0, 1) => .1
          case (1, 0) => .7
          case (1, 1) => .1
        }
      val l = if (rng.nextDouble() < p) 1 else 0
      net.learn(l)
      last_last_l = last_l
      last_l = l

    }
  }
  //======================================================================
   
  
  def layered() = {
    def initialize(t1: InternalNodePrototype, t2: InternalNodePrototype, t3: InternalNodePrototype) = {
      t1 observes LeafNodes.ObservationNode observes t1 
      t1 predicts LeafNodes.ObservationNode

      t2 observes LeafNodes.ObservationNode observes t1 observes t2 
      t2 predicts t1

      t3 observes LeafNodes.ObservationNode observes t1 observes t2 observes t3
      t3 predicts t2
    }
    val steps = 10000
    var turn = 0
    def getTurn(i: Int) = {
      if ((i + 1) % (steps / 3) == 0) {
        turn = (turn + 1) % 3
        println("now training " + (turn + 1))
      }
      turn
    }
    runNthOrder(2,steps, initialize, getTurn)
  }
  
  
  def layered2() = {
    def initialize(t1: InternalNodePrototype, t2: InternalNodePrototype, t3: InternalNodePrototype) = {
      t1 observes LeafNodes.ObservationNode observes t2 observes t3
      t1 predicts LeafNodes.ObservationNode

      t2 observes LeafNodes.ObservationNode observes t1  observes t3
      t2 predicts t1

      t3 observes LeafNodes.ObservationNode observes t1 observes t2 
      t3 predicts t2
    }
    val steps = 1000000
    var turn = 0
    def getTurn(i: Int) = {
      if ((i + 1) % (steps / 15) == 0) {
        turn = (turn + 1) % 3
        println("now training " + (turn + 1))
      }
      turn
    }
    runNthOrder(2,steps, initialize, getTurn)
  }
  
  
  
  
  def multipass() = {
    def initialize(t1: InternalNodePrototype, t2: InternalNodePrototype, t3: InternalNodePrototype) = {
      t1 observes LeafNodes.ObservationNode observes t1 observes t2 observes t3
      t1 predicts LeafNodes.ObservationNode

      t2 observes LeafNodes.ObservationNode observes t1 observes t2 observes t3
      t2 predicts t1

      t3 observes LeafNodes.ObservationNode observes t1 observes t2 observes t3
      t3 predicts t2 
    }
    val steps = 400000
    var turn = 0
    def getTurn(i: Int) = {
      if ((i + 1) % (steps / 20) == 0) {
        turn = (turn + 1) % 3
        println("now training " + (turn + 1))
      }
      turn
    }
    runNthOrder(0, steps, initialize, getTurn)
  }
  
  //------

  def runNthOrder(k : Int, steps: Int, initialize: (InternalNodePrototype, InternalNodePrototype, InternalNodePrototype) => Unit, getTurn: (Int) => Int) {
    val rng = new scala.util.Random(53)
    val alpha1 = 0.1
    val alpha2 = 0.01
    val alpha3 = 0.001
    val t1 = new InternalNodePrototype("Node1")
    val t2 = new InternalNodePrototype("Node2")
    val t3 = new InternalNodePrototype("Node3")

    def td1 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha1, 0, inDim, outDim, true)

      
    t1 withAnswerPredictor td1
    def td2 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha2, 0, inDim, outDim, true)
    t2 withAnswerPredictor td1
    def td3 = (inDim: Int, outDim: Int) => new MultivariateTD(alpha3, 0, inDim, outDim, true)
    t3 withAnswerPredictor td1
    initialize(t1, t2, t3)

    val net = new TDNet(t3, 1)
    net.showMonitor(steps/500)
    val node1: InternalNode = net.internalNodes("Node1")
    val node2: InternalNode = net.internalNodes("Node2")
    val node3: InternalNode = net.internalNodes("Node3")
    
    
    var last_l = 0
    var last_last_l = 0
    var turn = -1
    def setLearn(l1: Boolean, l2: Boolean, l3: Boolean) {
      node1.answerPredictor.asInstanceOf[MultivariateTD].alpha = if (l1) alpha1 else 0
      node2.answerPredictor.asInstanceOf[MultivariateTD].alpha = if (l2) alpha2 else 0
      node3.answerPredictor.asInstanceOf[MultivariateTD].alpha = if (l3) alpha3 else 0
    }

    for (i <- 0 until steps) {

      val p =
        if(k == 2){
        	(last_l, last_last_l) match {
        	case (0, 0) => .7
        	case (0, 1) => .3
        	case (1, 0) => .7
        	case (1, 1) => .3
        	}
        } else if(k == 1) {
          (last_l) match {
        	case 0 => .7
        	case 1 => .3
        	}          
        } else {//if(k==0
        	.7
        }
          
      getTurn(i) match {
        case 0 => setLearn(true, false, false)
        case 1 => setLearn(false, true, false)
        case 2 => setLearn(false, false, true)
        case _ => setLearn(true, true, true)
      }

      val l = if (rng.nextDouble() < p) 1 else 0
      net.learn(l)
      last_last_l = last_l
      last_l = l
    }
  }

  
  //------

}