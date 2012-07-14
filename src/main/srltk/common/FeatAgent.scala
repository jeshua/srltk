package srltk.common
import srltk.features.FeatureExtractor


abstract class FeatAgent[Obs <: FeatureObservation, Act <: IntAction]
		(val dd : DomainDescription,
		 val ex : Option[FeatureExtractor] = None) 
		extends Agent[Obs,Act] {
  
  val obs_dim = if(ex==None) dd.obs_dim else ex.get.length
  
  def observe(otm1: Feats, rm1: Double, atm1: Int, ot: Feats, rt: Double): Unit
  def observe(ot: Feats, rt: Double): Unit = ()
  def observe(otm2: Feats, rtm2: Double, atm2: Int, otm1: Feats, rtm1: Double, atm1: Int, ot: Feats, rt: Double): Unit = ()

  //==================================================
  //Translate act and observe from Agent  
    
  def observe(otm1: Obs, atm1: Act, ot: Obs): Unit = {
    observe(otm1.features,
      otm1.reward,
      atm1.n,
      ot.features,
      ot.reward)
  }
  override def observe(ot: Obs): Unit = {
    observe(ot.features,ot.reward)
  }
  override def observe(otm2: Obs, atm2: Act, otm1: Obs, atm1: Act, ot: Obs): Unit = {
    observe(
      otm2.features,
      otm2.reward,
      atm2.n,
      otm2.features,
      otm1.reward,
      atm1.n,
      ot.features,
      ot.reward)
  }
  
}