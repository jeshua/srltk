package srltk.common

abstract class Agent[Obs <: Observation, Act <: Action]
(val dai : DomainAgentInterface[_,Obs, Act]){
  def reset() //reset called for episodic 
  def act(o : Obs) : Act
  

  /**
   * In order to preempt common bugs when implementing agents, an Agent 
   * has three separate observation functions. This is to ensure the ordering of
   * actions and observations is clear.
   * 
   * Here are the three function:
   *  observe(o) called at the beginning of time and every step thereafter
   *  observe(o,a,o) called after the first transition (i.e. after second observation) and every step thereafter
   *  observe(o,a,o,a,o) called every timestep t > 2 (to make sarsa-like agents easy to implement)
   */
  //REQUIRED:  
  //observe: observation time-1, subsequent action taken at time-1, observation at time
  def observe(otm1 : Obs , atm1: Act, ot: Obs) : Unit 

  //OPTIONAL:  
  //observe: observation at time t
  def observe(ot: Obs) : Unit = ()
  
  //OPTIONAL:  
  //observe: observation time-1, subsequent action taken at time-1, observation at time
  def observe(otm2 : Obs, atm2: Act, otm1 : Obs , atm1: Act, ot: Obs) : Unit = ()

}
