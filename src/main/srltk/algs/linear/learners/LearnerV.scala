/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.algs.linear.learners

import srltk.common._
import srltk.algs.linear.agents.HasVFunction


abstract class LearnerV() extends Learner with HasVFunction{}
