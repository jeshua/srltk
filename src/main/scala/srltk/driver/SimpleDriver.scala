package srltk.driver
import srltk.common._
import srltk.domains._

class SimpleDriver[Obs <: Observation]
		(var d: SimDomain, 
				val ag: Agent[Obs,IntAction],
				val dai : DomainAgentInterface[SimState,Obs,IntAction]) {
  type St = SimState
  
  var prev_action: IntAction = null
  val a = new ManagedAgent(ag)
  
  def step(): (St, Obs, IntAction, Obs, Boolean) =
    {
      //agent observes current state and then acts
      val obs1 = dai.stateToObs(d.state);
      a.observe(prev_action, obs1)
      val action = a.getAction()
      var newEpisode = false
      prev_action = action

      d.state =
        if (!d.state.isAbsorbing)
          d.state.successor(action); //take action from non-absorbing state, just choose successor
        else {          
          a.flush(prev_action) //agent might need to observe the final action
          a.newEpisode() //reset agent's history
          prev_action = null
          newEpisode = true
          d.state.getInitial() //return start state
        }
      val obs2 = dai.stateToObs(d.state)
      (d.state, obs1, action, obs2, newEpisode)
    }
}
