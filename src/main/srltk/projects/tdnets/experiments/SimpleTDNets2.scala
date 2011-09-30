package srltk.projects.tdnets.experiments
import srltk.projects.tdnets._
import srltk.api.domain._
import srltk.api.agent._
import srltk.vis.ActivePlot
import scalala.tensor.dense.DenseVector

object SimpleTDNets2 {
	import LeafNodes._
  def main(args: Array[String]) {
    learnBoth()
  }

  //======================================================================

  def learnBoth() = {
    val alpha = .01
    val rng = new scala.util.Random(19)

    def td = (inDim: Int, outDim: Int) => new MultivariateTD(alpha, 0, inDim, outDim, true)
    val t1 = new InternalNodePrototype("Node1")
    t1 predicts ObservationNode withAnswerPredictor td observes ObservationNode
        
    val net = new TDNet(List(t1), 2)
    val node1: InternalNode = net.internalNodes("Node1")
    
    val steps = 3000
    net.showMonitor(steps/200)
    
    var p1 = 0d
    var p2 = 0d
    var lastl1 = 0
    var lastl2 = 0
    for (i <- 0 until steps) {
    	p1 = if(lastl2 == 0) .9 else .1
    	p2 = if(lastl1 == 0) .9 else .1
    	val l1 = if(rng.nextDouble() < p1) 1 else 0
    	val l2 = if(rng.nextDouble() < p2) 1 else 0
    	net.learn(DenseVector[Double](l1,l2))
    	lastl1 = l1
    	lastl2 = l2
    }
  }
  
}