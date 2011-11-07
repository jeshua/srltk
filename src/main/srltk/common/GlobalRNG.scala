package srltk.common



object GlobalRNG {
	private var rng = new util.Random
	def seed(n : Long) = {rng = new util.Random(n) }
	def nextInt(n : Int) = rng.nextInt(n)
	def nextDouble() = rng.nextDouble()
	def nextGaussian() = rng.nextGaussian()
}
