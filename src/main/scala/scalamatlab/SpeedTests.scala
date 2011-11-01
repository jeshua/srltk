package scalamatlab
import org.netlib.blas._
import scalala.tensor.dense._

import srltk.tools.utils.Timer._


object SpeedTests {

	def main(args : Array[String]){
		val ml = new ScalalaMatlab(false)
		println(BLAS.getInstance().getClass().getName)
	
		val blas = BLAS.getInstance()
		
		val rng = new util.Random
		def genVec(len : Int) : DenseVector[Double] = new DenseVectorCol[Double]((for(i <- 0 until len) yield rng.nextDouble()).toArray)
		val vecs = for(i <- 0 until 1000) yield genVec(4000)
		val vec = genVec(4000)
				
		var ret1 = 0d
		var time1=
			timeWallclock(
					() => {						
						for(j <- 0 until vecs.length ){
							var sum = 0d
							for(i <- 0 until vecs(j).length) sum += math.pow(vecs(j)(i)-vec(i),2)
							ret1 += math.sqrt(sum)
						}
					}
			)
		printf("ret1 = %f, time1 = %f\n",ret1,time1);
		
		var ret2 = 0d		
		var time2=
			timeWallclock(
					() => {
					  for(j <- 0 until vecs.length ){
						val sub = vecs(j)-vec
						ret2 += blas.dnrm2(sub.length,sub.data,1)
					  }
					}
			)
		printf("ret2 = %f, time2= %f\n",ret2,time2);
		
	}

}