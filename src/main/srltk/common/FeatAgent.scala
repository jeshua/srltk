package srltk.common
import srltk.common._
import scalala.tensor.dense.DenseVectorCol
import srltk.features.FeatureExtractor

abstract class FeatAgent(dd: DomainDescription, ex : FeatureExtractor = null) extends Agent(dd) {
  val numActions = dd.numActions
  val obsDim = if(ex == null) dd.obsDim else ex.length

  def toFeats(v: Traversable[Double]): Feats ={ 
		  val d = new DenseVectorCol[Double](v.toArray)
		  if(ex != null) ex(d)
		  else d
  }
  def toFeats(o: Observation): Feats = toFeats(o.vec)

  def act(f : Feats) : Int

  def observe(otm1: Feats, rm1: Double, atm1: Int, ot: Feats, rt: Double): Unit
  def observe(ot: Feats, rt: Double): Unit = ()
  def observe(otm2: Feats, rtm2: Double, atm2: Int, otm1: Feats, rtm1: Double, atm1: Int, ot: Feats, rt: Double): Unit = ()

  //==================================================
  //Translate act and observe from Agent
  
  def act(o : Observation) : Action = IntAction(act(toFeats(o.vec)))
    
  def observe(otm1: Observation, atm1: Action, ot: Observation): Unit = {
    observe(toFeats(otm1.vec),
      otm1.reward,
      atm1.asInstanceOf[IntAction].n,
      toFeats(ot.vec),
      ot.reward)
  }
  override def observe(ot: Observation): Unit = {
    observe(toFeats(ot.vec),
      ot.reward)
  }
  override def observe(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation): Unit = {
    observe(
      toFeats(otm2.vec),
      otm2.reward,
      atm2.asInstanceOf[IntAction].n,
      toFeats(otm1.vec),
      otm1.reward,
      atm1.asInstanceOf[IntAction].n,
      toFeats(ot.vec),
      ot.reward)
  }

}

