package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._

//test policy is an agent so we can run it independently 
abstract class TestPolicy(dd : DomainDescription) extends Agent(dd) {
  def getValue(s: SimState): Double
  def getInitialState(): SimState
  def getAction(s: SimState): IntAction

  //agent functions
  def observe(otm1: Observation, atm1: Action, ot: Observation): Unit = ()
  //choose action from current observation
  def act(o: Observation): Action = getAction(o.asInstanceOf[SimObservation].simState)
}
