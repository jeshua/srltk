package srltk.projects.psrs
import srltk.projects.psrs.pomdps._
import srltk.vis.ActivePlot
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.util.Random
import srltk.tools.utils._
import java.util.Scanner
    import scala.math._

class EvaluateDiscreteTPSR(tpsr : DiscreteTPSR, 
                           sim : POMDPSimulator, 
                           length : Int = 1, 
                           rng : Random = new Random(),
                           debug : Boolean = false) 
{


    val numObservations = sim.numObservations
    val numActions = sim.numActions

    tpsr.reset()
    sim.reset() 
    
    val numTimesteps = 6000
    var samples = new ArrayBuffer[(Int,Int)] 

    var testErr = 0d 
    var testLL = 0d
    var testL = 0d
    var testLLCount = 0

    var realLL = 0d
    var realL = 0d
    var realLLCount = 0

    var randErr = 0d
    var randLL = 0d
    var randL = 0d

    var b = sim.uniformSteadyState()
    val queue = new Queue[(Int,Int)]
    var count = 0d
    
    for(t <- 0 until numTimesteps){
      val a = rng.nextInt(numActions)
      val state = sim.currentState
      val o = sim.act(a)
      samples.append((a,o))
      queue.enqueue((a,o))
      if(queue.length >= length){
        count += 1
        //---
        val realProb = sim.beliefPredict(b,queue.toArray)
        val testProb = tpsr.predict(queue.toArray)
        val randProb = (for(i <- 0 until queue.length) yield 1d/numObservations).foldLeft(1d)(_ * _)
        //---
        //calc error
        testErr = (scala.math.pow(realProb-testProb,2)+count*testErr)/(count+1)
        randErr = (scala.math.pow(realProb-1d/numObservations,2)+count*(randErr))/(count+1)
        randLL = (log(randProb) + count*randLL) / (count+1)
        randL = (randProb + count*randL) / (count+1)
        if(realProb > 0){
          realLLCount += 1
          realLL = (log(realProb) + realLLCount*realLL) / (realLLCount+1)
          realL = (realProb + realLLCount*realL) / (realLLCount+1)
        }
        if(testProb > 0){
          testLLCount += 1
          testLL = (log(testProb) + testLLCount*testLL) / (testLLCount+1)
          testL = (testProb + testLLCount*testL) / (testLLCount+1)
        }

        //update state
        val sample = queue.dequeue()
        val a = sample._1
        val o = sample._2
        b = sim.beliefUpdate(b,a,o)
        tpsr.observe(a,o)
      }      
    }      
    val testZeros = count - testLLCount
    val realZeros = count - realLLCount
}

object EvaluateDiscreteTPSR{

  def main(args : Array[String]){
    val evaluationLength = 1

    val pomdps = List(
      (new Tiger,"Tiger",2,10),
      (new Paint,"Paint",2,10),
      (new FloatReset,"FloatReset",10,10),
      (new Shuttle,"Shuttle",10,10))
    var count = 0

    val plotMSE = new ActivePlot(evaluationLength+"-step evaluation","Samples","MSE")
    val plotLL = new ActivePlot(evaluationLength+"-step evaluation","Samples","Log Likelihood Ratio")
    plotMSE.setYLog()
    plotMSE.setXLog()
    //plotLL.setYLog()
    plotMSE.display()
    plotMSE.setMarkersEnabled(true)
    plotLL.display()
    plotLL.setLocation(500,0)
    plotLL.setMarkersEnabled(true)
  plotLL.setXLog()
    for(p <- pomdps) {        
      val pomdp = p._1
      val name = p._2
      val maxrank = p._3
      val s = p._4
      val samples = (for(i <- 6 until s) yield (math.pow(2,i)*200).toInt).toList
      plotMSE.newDataset(name)
      plotLL.newDataset(name)
      val na = pomdp.numActions
      val no = pomdp.numObservations
      
      for(numSamples <- samples){
        val rng = new scala.util.Random(15)
        val samples = pomdp.generateSamples(numSamples,rng)
        val t = Discover(3,100,samples,rng)      
        val tests = t
        val h = Discover(3,100,samples,rng)
        val hists = h
        //val hists = DiscretePSRUtils.histsOfLength(na,no,4)
        printf("There are %d tests and %d histories\n",tests.length,hists.length)

        val db = new DiscreteSampleDatabaseAC(pomdp.numActions, 
                                              pomdp.numObservations, 
                                              hists,tests)
        printf("Updating counts... ")
        def f() = db.append(samples)
        printf("done in %.0f ms\n",Timer.time(f))

        val numTrials = 5
        
        var mseSum = 0d
        var llSum = 0d
        def f2() = {
          for(i <- 0 until numTrials){
            val m = new DiscreteTPSR(db,maxRank=maxrank,tpsr=true)
            //val m = new DiscreteTPSR(db,0.1)
            m.learn()
            println("Rank is "+m.estimatedRank)
            val res = new EvaluateDiscreteTPSR(m, pomdp, evaluationLength,rng)
            mseSum += res.testErr
            llSum += res.realLL/res.testLL
          }
        }
        val time = Timer.time(f2)
        val x = numSamples
        printf("Ran %d trials of %s with %d samples in %.0fms\n",numTrials,name,x,time)
        plotMSE.addPointToSeries(x,mseSum/numTrials,count)
        plotLL.addPointToSeries(x,llSum/numTrials,count)     
      }
      count +=1
    }

  }
}

