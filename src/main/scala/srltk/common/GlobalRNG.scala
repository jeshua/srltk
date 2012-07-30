package srltk.common

object GlobalRNG {
	private var rng = new util.Random
	def seed(n : Long) = {rng = new util.Random(n) }
	def nextInt(n : Int) = rng.nextInt(n)
	def nextDouble() = rng.nextDouble()
	def nextGaussian() = rng.nextGaussian()
    def random = rng
    
    def sampleMultinomial(p : Seq[Double]) : Int = {
	  val dart = nextDouble();
	  var sum = 0d;
	  var i = 0
	  while(sum < dart){
	    sum+=p(i)	    
	    i+=1;
	  }
	  i-1
	}
}
