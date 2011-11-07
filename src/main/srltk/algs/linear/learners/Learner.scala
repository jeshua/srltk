package srltk.algs.linear.learners

import srltk.common._

trait Learner {
	def reset() : Unit
	def learn(otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double) : Unit
	def learn(otm1: Feats, rtm1 : Double) : Unit = ()
	def learn(otm2: Feats, rtm2 : Double, atm2: Int,otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double) : Unit = ()
}
