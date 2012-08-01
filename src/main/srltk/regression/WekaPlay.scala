package srltk.regression

import scalala.tensor._
import scalala.tensor.dense._
import weka.core._
import weka.classifiers.functions.GaussianProcesses
import weka.datagenerators.classifiers.regression.MexicanHat
import scala.collection.mutable.ArrayBuffer
   
   
object WekaPlay {
  
  def vecToInstance(vec : DenseVector[Double]) : Instance = {
    import weka.core.Instance    
    val ret = new DenseInstance(1.0,vec.data)    
    ret
  }
  
  
 def main(args : Array[String]) = {
   
   
   val dims = 2
   val numPoints = 100
   val points = new ArrayBuffer[DenseVector[Double]]
   for(i <- 0 until numPoints){
     
   }
   
   
   val data = new Instances("",null,numPoints)
   val gp = new GaussianProcesses
   for(i <- 0 until points.length){
     
   }
   
   
 }
}