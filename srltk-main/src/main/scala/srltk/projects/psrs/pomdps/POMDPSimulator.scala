package srltk.projects.psrs.pomdps
import scala.util.Random
import scala.util.control.Breaks._
import java.io.{BufferedOutputStream, File, FileOutputStream}
import scalala.tensor.dense._
import scalala.library.LinearAlgebra._
import srltk.projects.psrs._
import scala.collection.mutable._

trait POMDPSimulator extends POMDPSpec {
  var random = new Random()
  var currentState : Int = 0 
  
  var computeBeliefState : Boolean = true
  var b : DenseVectorCol[Double] = null
  var b1 : DenseVectorCol[Double] = null

  def beliefStartState() : DenseVectorCol[Double]= {
    val b1 = DenseVector.zeros[Double](numStates)
    for(s <- 0 until numStates)
      b1(s) = startStateProb(s)
    b1
  }

  def uniformPolicy() : DenseMatrix[Double] = {
    val pi = DenseMatrix.ones[Double](numStates,numActions)
    pi :* (1d/numActions)
  }

  //generate samples from uniform policy
  def generateSamples(n : Int, rng : scala.util.Random) = {
    var samples = new ArrayBuffer[(Int,Int)]
    for(i <- 0 until n){
      val a = rng.nextInt(numActions)
      val o = act(a)
      samples.append((a,o))
    }
    samples
  }


  def uniformSteadyState() = beliefSteadyState(uniformPolicy())
  //policy is a transition matrix numStates X numActions
  def beliefSteadyState(policy : DenseMatrix[Double]) : DenseVectorCol[Double] = {
    require(policy.numCols == numActions)
    require(policy.numRows == numStates)
    //Taij = pr(s2=j|a,s1=i)
    //pi   = pr(a | s1)
    //pr(s2=j|s1=i) = sum_a Taij p(s2=j|s1=i,a) * pi(a|s1=i)
    val T = DenseMatrix.zeros[Double](numStates,numStates)
    for(a  <- 0 until numActions) {
      val Ta = getTMat(a)
      for(s1 <- 0 until numStates;
          s2 <- 0 until numStates)
        T(s1,s2) += Ta(s1,s2) * policy(s1,a)
    }
    //we want Tb* = b*
    //choose steady state as first eigenvector of T
    val res = eig(T.t)
    //get first eigenvector
    val bstar = DenseVectorCol.zeros[Double](numStates)
    bstar := res._3(0 until numStates,0)
    bstar :/ bstar.sum
  }

  def beliefO(action : Int, obs : Int) : Double = {
    beliefO(b,action,obs)
  }
  def beliefO(b : DenseVectorCol[Double], action : Int, obs : Int) : Double = {
    beliefPredict(b,Array((action,obs)))
  }
  //sequence of (a,o) pairs
  def beliefPredict(b : DenseVectorCol[Double], sequence : Array[(Int,Int)]) : Double = {    
  
    var m = DenseMatrix.eye[Double](numStates,numStates)
    for(sample <- sequence){
      val a = sample._1
      val o = sample._2
//      sum_j p(j|i,a)*p(o|i,j,a)
      m = m * (getTMat(a) :* getOMat(a,o))
    }    
    //b: n x 1, m: nxn
    val ret = (m.t * b).sum
    ret

  }
  //calculate matrix with entries p(t,h)
  def beliefJointPTH(
    b : DenseVectorCol[Double],
    tests : Array[Array[(Int,Int)]], 
    hists : Array[Array[(Int,Int)]]) : DenseMatrix[Double] = {
      val PTH = DenseMatrix.zeros[Double](tests.length,hists.length)
      for(t <- 0 until tests.length;
          h <- 0 until hists.length)
        PTH(t,h) = beliefPredict(b,hists(h) ++ tests(t))
      PTH :/ PTH.sum
    }
  //calculate vector with entries p(h)
  def beliefPH(
    b : DenseVectorCol[Double],
    hists : Array[Array[(Int,Int)]]) : DenseVectorCol[Double] = {
      val PH = DenseVectorCol.zeros[Double](hists.length)
      for(h <- 0 until hists.length)
        PH(h) = beliefPredict(b,hists(h))
      PH :/ PH.sum
    }

  def beliefUpdate(b : DenseVectorCol[Double], action : Int, obs : Int) : DenseVectorCol[Double] = {
    val m = b.t * (getTMat(action) :* getOMat(action,obs))
    val Z = m.sum
    if(Z == 0)
      (m :* 0).t
    else
      (m :/ Z).t
  }


  def reset() = {
    currentState = sample(startStateProb _, numStates)
    this.b = beliefStartState()
    this.b1 = beliefStartState()
  }

  def sample(prob: (Int) => Double, n: Int): Int = {
    val dart = random.nextDouble()
    var sum = 0d
    var ret = -1
    breakable {
      for (i <- 0 until n) {
        sum += prob(i)
        if (dart < sum) { ret = i; break }
      }
    }
    if(ret < 0)
      throw new IllegalArgumentException("Probabilities didn't sum to 1, sum is "+sum+", n is "+n)
    ret
  }

  //returns observation
  def act(action: Int): Int = {
    val lastState = currentState
    def t(s: Int) = T(currentState, action, s)
    currentState = sample(t _, numStates)
    def o(i: Int) = O(lastState, action, currentState, i)
    val obs = sample(o _, numObservations)
    
    b = beliefUpdate(b,action,obs)
    obs
  }

  def outputSamples(numSamples : Int, file : String) : Unit = {
    val samples = DiscretePSRUtils.generateTrajectory(numSamples, this)
    outputSamples(samples,file)
  }
  def outputSamples(samples : Array[(Int,Int)], file : String) : Unit = {
    try
    {
      
      val ret = new StringBuilder()
        for(i <- 0 until samples.length){
          val s = samples(i)
          ret ++= s._1+"/"+s._2
          if( i < samples.length-1) ret ++= ";"
        }

      ret++="\n"
      
      var out_file = new java.io.FileOutputStream(file) 
      var out_stream = new java.io.PrintStream(out_file) 
      out_stream.print(ret.toString) 
      out_stream.close 
    } catch{
      case e : Exception => e.printStackTrace()
    }
  }
}

