package srltk.projects.psrs
import srltk.projects.psrs.pomdps._
import srltk.vis.ActivePlot
import scala.collection.mutable.Queue
import scala.util.Random
import srltk.tools.utils._
import java.util.Scanner

object TimeDiscreteTPSR {

  def main(args : Array[String]){
    val evaluationLength = 5
    val pomdps = List(
      (new Tiger,"Tiger",2),
      (new Paint,"Paint",2),
      (new FloatReset,"FloatReset",5),
      (new Shuttle,"Shuttle",7))
    var count = 0
    val numSamples = 1000000
    val maxTestLength = 3
    val maxTests = 50
    //pause for user input
    val pause = new Scanner(System.in)  
    println("Enter to start: ")
    pause.nextLine()
    for(p <- pomdps) {        
      val pomdp = p._1
      val name = p._2
      println("------------------------------")
      println(name)
      val rank = p._3
      val na = pomdp.numActions
      val no = pomdp.numObservations
      val rng = new scala.util.Random()
      val samples = pomdp.generateSamples(numSamples,rng)
      val testLength = maxTests/maxTestLength

      val t = Discover(maxTestLength,testLength,pomdp.generateSamples(numSamples/1000,rng),rng)
      val tests = t
      val hists = t
      printf("%d tests and %d histories, %d samples\n\n",tests.length,hists.length,numSamples)

      /*println("NAIVE: ")
      def time1 = {
      val db = new DiscreteSampleDatabase(pomdp.numActions, 
                                               pomdp.numObservations, 
                                               hists,tests)
        printf("Updating counts... \t\t")
        def f1() = db.append(samples)
        var time = Timer.time(f1)
        printf("done in %.0f ms\n",time)

        val numTrials = 1
        val m = new DiscreteTPSR(db,rank=rank)
        def f2() =  m.learn()       
        printf("Learning TPSR... \t\t")
        time = Timer.time(f2)
        printf("done in %.0f ms\n",time)
      }
      time1*/
      //--------------------------------------------------

      def time2(tpsr : Boolean) = {
        if(tpsr)
          println("TPSR: ")
        else
          println("PSR: ")
        val db = new DiscreteSampleDatabaseAC(pomdp.numActions, 
                                              pomdp.numObservations, 
                                              hists,tests)
        printf("Updating counts... \t\t")
        def f1() = db.append(samples)
        var time = Timer.time(f1)
        printf("done in %.0f ms\n",time)

        val numTrials = 1
        val m = new DiscreteTPSR(db,maxRank=rank,tpsr=tpsr)
        def f2() =  m.learn()
        printf("Learning PSR... \t\t")
        time = Timer.time(f2)
        printf("done in %.0f ms\n",time)
//        val res = new EvaluateDiscreteTPSR(m, pomdp, 1, rng,true)
      }
      time2(false)
      time2(true)

    }
  }
}
