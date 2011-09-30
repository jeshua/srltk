package srltk.projects.psrs.test
import srltk.projects.psrs.pomdps._
import srltk.vis.ActivePlot
import scala.collection.mutable.Queue
import scala.util.Random
import srltk.tools.utils._
import java.util.Scanner
import srltk.projects.psrs._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.collection.mutable.ArrayBuffer
import scalax.io._

object TestRanksTPSR {

  
  val learnPSRDir = "/home/jeshua/Dropbox/linearPSRs/drivers"  
  val maxSeqLength = 7
  
  def getLinPSRLikelihood(pomdp: POMDPSimulator, rank : Int, samples : ArrayBuffer[(Int,Int)], testSamples : ArrayBuffer[(Int,Int)]) : (Double,Double) = {
    import scala.sys.process._
    import java.util.regex._
    val rng = new scala.util.Random()
    val tmpdir = System.getProperty("java.io.tmpdir")+"/linPSR"+rng.nextInt(100000)
    "mkdir -p "+tmpdir !
    val tmpDataFile = tmpdir + "/data"
    val tmpPSRFile = tmpdir + "/psr"
    pomdp.outputSamples(samples.toArray,tmpDataFile)
    val cmd = learnPSRDir + "/learnLinearPSR "+tmpDataFile +" "+pomdp.numActions+" 1 "+pomdp.numObservations+" "+maxSeqLength+" 1 "+tmpPSRFile+" "+rank
    println(cmd)
    cmd !! ;

    pomdp.outputSamples(testSamples.toArray,tmpDataFile)
    val cmd2 = learnPSRDir + "/evalLinearPSR "+tmpDataFile +" "+tmpPSRFile
    println(cmd2)
    val lines : String = cmd2 !!;
    val split = lines.split("\n")
    val last = split(split.length-1)
    val vs = last.split(",")
    require(vs.length==4)
    println(last)
    (vs(2).toDouble,vs(3).toDouble) //LL, L
  }
  
  def main(args : Array[String]){
    test(args(0).toDouble.toInt)
  }
  
  def test(ns : Int) {
    val evaluationLength = 1
    val numTrials = 15
    val pomdps = List(
      (new Tiger,"Tiger",3,ns),
      (new Paint,"Paint",3,ns),
      (new FloatReset(4),"FloatReset",4,ns),
      (new Chain(4),"Chain",4,ns),
      (new Shuttle,"Shuttle",8,ns))
    var count = 0
    //val plotLL = new ActivePlot("Likelihood","Samples","Log Likelihood Ratio")
    //plotLL.display()
    //plotLL.setYLog()
    //plotLL.setMarkersEnabled(true)
    
    for(p <- pomdps) {        
      try{
      val pomdp = p._1
      val name = p._2
      println("Starting "+name)
      val maxrank = p._3
      val s = p._4

      val rng = new scala.util.Random(38974)
      var realLL = 0d
      var realL = 0d
      var randLL = 0d
      var randL = 0d
      var testLs = List[List[Double]]()
      var testLLs = List[List[Double]]()
      var linPSRLs = List[List[Double]]()
      var linPSRLLs = List[List[Double]]()

      //printf("There are %d samples, %d tests and %d histories\n",s.toInt,tests.length,hists.length)
      
//      plotLL.newDataset(name+"_tpsr")
  //    plotLL.newDataset(name+"_lin")
      
      //      "./learnLinearPSR ~/FloatReset.data 2 1 2 5 1 float.psr 4"

      //loop over ranks
      for(rank <- 1 to maxrank){
        var testL = List[Double]()
        var testLL = List[Double]()
        var linPSRLL = List[Double]()
        var linPSRL = List[Double]()
        realL = 0d
        realLL = 0d
	//loop over trials
	for(i <- 0 until numTrials){
          val samples = pomdp.generateSamples(s.toInt,rng)
          val tests = Discover(maxSeqLength,200,samples,rng)
          val hists = Discover(maxSeqLength,200,samples,rng)
          val db = new DiscreteSampleDatabaseAC(pomdp.numActions, 
                                                pomdp.numObservations, 
                                                hists,tests)
          db.append(samples)
          

	  val m = new DiscreteTPSR(db,-1,rank,tpsr=true)
	  m.learn()
	  val res = new EvaluateDiscreteTPSR(m, pomdp, evaluationLength, rng)
	  realL = (res.realL + i * realL)/(i+1)
	  realLL = (res.realLL + i * realLL)/(i+1)
	  randL = res.randL
	  randLL = res.randLL
	  testL ::= res.testL
	  testLL ::= res.testLL
          val lin = getLinPSRLikelihood(pomdp,rank,samples,res.samples)
          linPSRLL ::= lin._1
          linPSRL ::= lin._2
	}
	testLs ::= testL.reverse
        testLLs ::= testLL.reverse
        linPSRLLs ::= linPSRLL.reverse
        linPSRLs ::= linPSRL.reverse
      }
      //output data 
      val json = 
      ("rankExp_"+name -> 
       ("pomdp" -> name) ~
       ("ranks" -> (1 to maxrank).toList) ~
       ("evalLength" -> evaluationLength) ~
       ("numSamples" -> s.toInt) ~
       ("realL" -> realL) ~
       ("realLL" -> realLL) ~
       ("randL" -> randL) ~
       ("randLL" -> randLL) ~
       ("testL" -> testLs.reverse) ~
       ("testLL" -> testLLs.reverse)~
       ("linPSRL" -> linPSRLs.reverse)~
       ("linPSRLL" -> linPSRLLs.reverse))     

      val output:Output = Resource.fromFile("/home/jeshua/"+name+"_"+s.toInt+".json")
      output.write(compact(render(json)))
      }  catch{ case e : Exception => e.printStackTrace() }
      count +=1     
    }
  }
}

