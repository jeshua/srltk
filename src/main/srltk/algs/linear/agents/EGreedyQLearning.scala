package srltk.algs.linear.agents
import srltk.common._
import srltk.algs.linear.learners._
import srltk.features.FeatureExtractor

class EGreedyQLearning(
		dd: DomainDescription,
		alpha: Double = 0.05,
		epsilon: Double = 0.1,
		lambda: Double = 0,
		gamma: Double = 0.99,
		initialValue: Double = 0,
		ex : FeatureExtractor = null)   
  extends
  
  EGreedyAgent(
      dd,
      new QLearning(dd.numActions,if(ex==null) dd.obsDim else ex.length,
    		  alpha, lambda, gamma, initialValue),
      epsilon,
      ex)
