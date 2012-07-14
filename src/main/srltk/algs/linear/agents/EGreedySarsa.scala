package srltk.algs.linear.agents
import srltk.common._
import srltk.algs.linear.learners._
import srltk.features.FeatureExtractor


class EGreedySarsa[Obs <: FeatureObservation, Act <: IntAction](
   dd : DomainDescription,
  alpha: Double = 0.05,
  epsilon: Double = 0.1,
  lambda: Double = 0,
  gamma: Double = 0.99,
  initial_value: Double = 0,
  ex : Option[FeatureExtractor] = None) 
  
  extends
  
  EGreedyAgent(
      dd,
      new Sarsa(dd.num_actions,if(ex==None) dd.obs_dim else ex.get.length,
    		  alpha, lambda, gamma, initial_value),
      epsilon,
      ex)
