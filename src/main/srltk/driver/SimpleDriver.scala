package srltk.driver
import srltk.common._
import srltk.domains._

class SimpleDriver(var d: SimDomain, val ag: Agent) {
  var prevAction: Action = null
  val a = new ManagedAgent(ag)
  def step(): (SimState, SimObservation, Action, SimObservation, Boolean) =
    {
      //agent observes current state and then acts
      val obs1 = d.state.observation()
      a.observe(prevAction, obs1)
      val action = a.getAction()
      var newEpisode = false
      prevAction = action

      d.state =
        if (!d.state.absorbing)
          d.state.successor(action); //take action from non-absorbing state, just choose successor
        else {
          val newObs = new SimObservation(obs1.vec, 0, obs1.simState) //observation of absorbing state w/o reward
          a.observe(prevAction, newObs) //so agent observes absorbing state again
          a.newEpisode() //reset agent's history
          prevAction = null
          newEpisode = true
          d.state.getInitial() //return start state
        }
      val obs2 = d.state.observation
      (d.state, obs1, action, obs2, newEpisode)
    }
}
