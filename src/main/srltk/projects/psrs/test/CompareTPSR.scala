package srltk.projects.psrs.test
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder
import srltk.projects.psrs._
import srltk.projects.psrs.pomdps._
import srltk.vis.ActivePlot
import scala.util.Random
import srltk.tools.utils._
import scala.sys.process._


object CompareTPSR{  
  
  
  
  
  def main(args : Array[String]){
    
    val lines : String = "find /home/jeshua/Eclipse/srltk -name *.scala" !! ;
    printf("%s",lines)
    
  }
  def eval() {
    val evaluationLength = 1
    val pomdps = List(
      (new Tiger,"Tiger",50000),
      (new Paint,"Paint",50000),
      (new FloatReset,"FloatReset",50000),
      (new Shuttle,"Shuttle",200000))
    
    for(p <- pomdps) {   
    	val pomdp = p._1
      val name = p._2
      val n = p._3
      pomdp.outputSamples(n,"/home/jeshua/"+name+".data")
    }
}
}

