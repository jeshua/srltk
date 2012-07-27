package srltk.algs.linear.learners
import srltk.algs.linear.common._

trait QFunction {
	//def getQ(o : Vec) : Vec
	def numActions() : Int
	//@TODO fix this
	def getMaxA(o: Vec): Int = 1;//getQ(o).argmax
}