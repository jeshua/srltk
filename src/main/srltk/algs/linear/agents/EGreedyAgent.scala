package srltk.algs.linear.agents
import srltk.common._
import srltk.algs.linear.learners._
import srltk.algs.linear.policies._
import srltk.features.FeatureExtractor

//simple agent gluing together a learner with e-greedy
class EGreedyAgent[Obs <: FeatureObservation, Act <: IntAction](
    val dd : DomainDescription,
    val learner : LearnerQ,
    val epsilon : Double,
    val ex : Option[FeatureExtractor] = None) 
    extends FeatAgent[Obs,Act](dd,ex) with HasQFunction {

  def getQ(o : Feats, a : Int) = learner.getQ(o,a) 
  def getMaxA(o : Feats) = learner.getMaxA(o)
  
  val policy = new EpsilonGreedy(dd.num_actions,epsilon, learner.getQ _);
  
  def enableExploration() = policy.enableExploration()
  def disableExploration() = policy.disableExploration()
  
  // choose policy's action
  override def act(o: Feats) = policy.act(o);
  // repeated observation
  def observe(otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double): Unit = ()
  override def observe(otm2: Feats, rtm2 : Double, atm2: Int, otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double): Unit =
    learner.learn(otm2,rtm2,atm2,otm1,rtm1,atm1,ot,rt)
    
    def reset() = learner.reset()
}
