/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.common

class DomainDescription(
    val numActions : Int,
    val obsDim : Int = -1,
    val numObservations : Int = -1
) {}


abstract class Agent(val dd : DomainDescription)		    
		    extends CanObserve {
  def reset() //reset called for episodic 
  def act(o : Observation = null) : Action
}
