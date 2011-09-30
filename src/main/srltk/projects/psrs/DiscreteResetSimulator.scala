package srltk.projects.psrs

trait DiscreteResetSimulator {
	def numActions : Int
	def numObservations : Int
	def reset() : Unit
	def currentObservation() : Int
	def takeAction(action : Int)
}