package srltk.driver
import srltk.common._

/**
 *
 * I've taken a new approach to agent structure, these choices are motivated
 * by shortcomings in other RL libraries. Namely, much code in agent design
 * is repeated and easily implemented incorrectly:
 *    1) storing historical actions and observations for the first three
 *       timesteps. For example, SARSA cannot learn until the 3rd timestep
 *       but when implementing it, it is easy to store and update with the wrong
 *       set of actions
 *          to alleviate this, we provide 3 'observe' functions
 *          observe(ot) [called every timestep t]
 *          observe(otm1,atm1,ot) [called every timestep t > 1]
 *          observe(otm2,atm2,otm1,atm1,ot) [called every timestep t > 2]
 *    2) observing actions taking in the world for off policy learning
 *          we solve this by passing the true previous actions on each timestep
 *          rather then whatever was chosen by the agent at the last timestep
 */

class ManagedAgent(val agent : Agent)  {  
  private val history = new ManagedHistory(3)
  private var timestep = 0
  private var myLastAction: Action = null
  final def newEpisode() {
    history.clear()
    timestep = 0
    agent.reset()
  }
  //Called every timestep by driver;
  //on first timestep prevAction is expected to be null
  final def observe(prevAction: Action, o: Observation): Unit =
    {
      //append 
      if (prevAction != null)
        history.append(prevAction)
      history.append(o)
      //observe
      if (history.length >= 1)
        agent.observe(history.o_t());
      if (history.length >= 2)
        agent.observe(history.o_t(-1), history.a_t(-1), history.o_t());
      if (history.length > 2)
        agent.observe(history.o_t(-2), history.a_t(-2),
          history.o_t(-1), history.a_t(-1), history.o_t());
      timestep += 1
    }

  //external action function
  def getAction(): Action = agent.act(history.o_t())  
}
