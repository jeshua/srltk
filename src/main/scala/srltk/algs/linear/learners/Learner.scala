package srltk.algs.linear.learners

import srltk.common._
import srltk.algs.linear.VecObs
import srltk.algs.linear.common._

trait Learner {
	def reset() : Unit
	def learn(otm1: Vec, rtm1 : Double, atm1: Int, ot: Vec, rt : Double) : Unit
	def learn(otm1: Vec, rtm1 : Double) : Unit = ()
	def learn(otm2: Vec, rtm2 : Double, atm2: Int,otm1: Vec, rtm1 : Double, atm1: Int, ot: Vec, rt : Double) : Unit = ()
}
