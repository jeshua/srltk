
package srltk.common

/**
 * Trait CanObserve 
 * 
 * In order to preempt common bugs when implementing agents, the CanObserve trait
 * has three separate observation functions. This is to ensure the ordering of
 * actions and observations is clear
 * 
 * Here are the three function, **only the second is required**:
 *  observe(o) called at the beginning of time and every step thereafter
 *  observe(o,a,o) called after the first transition (i.e. after second observation) and every step thereafter
 *  observe(o,a,o,a,o) called every timestep t > 2 (to make sarsa-like agents easy to implement)
 *
 *  otm1 should be read as action at time t-1
 *  atm1 should be read as action at time t-1
 *  etc.
 */
trait CanObserve {
  //REQUIRED:  
  //observe: observation time-1, subsequent action taken at time-1, observation at time
  def observe(otm1 : Observation , atm1: Action, ot: Observation) : Unit 

  //OPTIONAL:  
  //observe: observation at time t
  def observe(ot: Observation) : Unit = ()
  
  //OPTIONAL:  
  //observe: observation time-1, subsequent action taken at time-1, observaton at time
  def observe(otm2 : Observation , atm2: Action, otm1 : Observation , atm1: Action, ot: Observation) : Unit = ()
}



