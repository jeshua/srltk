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
	
	
	def randIdx(min : Int, max : Int) : Array[Int] = {
	  val idx = (min until max).toList.toArray	  
	  val N = max-min;
	  val ret = new Array[Int](N)
	  var n = 0;
	  while(n < N){
	    val dart = nextInt(N-n);
	    ret(n) = idx(dart);	    
	    idx(dart) = idx(N-n-1)
	    n+=1;	    
	  }	  
	  ret	  
	}
	
	
	
}
