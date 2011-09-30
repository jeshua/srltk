package srltk.projects.psrs
import scalala.tensor.dense._
import scalala.library.LinearAlgebra._

class DiscreteTPSR(val db: DiscreteSampleDatabase, 
                   val sigmThresh : Double = 0.001,
                   val maxRank: Int = -1,
                 val tpsr : Boolean = true) {

  val numHistories = db.histories.length
  val numTests = db.tests.length
  val numActions = db.na
  val numObservations = db.no

  //model parameter
  var mStar: DenseVectorCol[Double] = null
  var mInfinity: DenseVectorCol[Double] = null
  var Mao: Array[DenseMatrix[Double]] = null

  var estimatedRank : Int = 0

  //state
  var state: DenseVectorCol[Double] = null

  def reset() = {
    state = mStar
  }

  def observe(action: Int, observation: Int) = {
    if (state == null) reset()
    val ao = action * numObservations + observation
    val extension = Mao(ao) * state
    val denom = mInfinity.t * extension

    state = extension / denom
  }

  def predict(a: Int, o: Int): Double = {
    val m = a * numObservations + o
    val products = for (j <- 0 until numObservations)
      yield mInfinity.t * Mao(a * numObservations + j) * state
    val denom = products.sum
    val numer = products(o)
    val ret = numer / denom
    //if(ret > 1) 1 else if(ret < 0) 0 else ret
    ret
  }

  //returns Pr(test|state)
  def predict(test: Array[(Int, Int)]): Double = {
    def sampleToIndex(sample: (Int, Int)) = sample._1 * numObservations + sample._2
    var B = Mao(sampleToIndex(test(0)))
    for (i <- 1 until test.length) B = B * Mao(sampleToIndex(test(i)))
    mInfinity.t * B * state
  }

  /*We will use the following dimensions
   * m = # of tests 
   * n = # of histories
   * k = target rank
   */

  //==================================================

  def computePH(normalize: Boolean = true): DenseVectorCol[Double] = {
    val PH = DenseVector.zeros[Double](numHistories)
    for (h <- 0 until numHistories) {
      PH(h) = db.hCount(h)
    }
    if (normalize) {
      val sum = PH.sum
      for (h <- 0 until numHistories) PH(h) /= sum
    }
    PH
  }

  //==================================================

  def computePT(): DenseVectorCol[Double] = {
    val PT = DenseVector.zeros[Double](numTests)
    for (t <- 0 until numTests) {
      val denom = db.samples.length - db.tests(t).length + 1
      PT(t) = if(denom ==0) 0 else db.getAttemptCount(t).toDouble / denom
    }
    PT
  }


  //==================================================

  def computePTH(): DenseMatrix[Double] = {
    val PTH = DenseMatrix.zeros[Double](numTests, numHistories)
    val PT = computePT()
    for (h <- 0 until numHistories) {
      for (t <- 0 until numTests) {
        val denom = db.samples.length - db.tests(t).length + 1
        PTH(t,h) = if(denom == 0) 0 else db.htCount(h,t).toDouble / (PT(t)*denom)
      }
    }
    PTH
  }

  //==================================================

  def computePTgivenH(): DenseMatrix[Double] = {
    val PTgivenH = DenseMatrix.zeros[Double](numTests, numHistories)
    for (h <- 0 until numHistories) {
      for (t <- 0 until numTests) {
        val denom = db.hatCount(h,db.taIndex(t))
        PTgivenH(t,h) = if(denom == 0) 0 else db.htCount(h,t).toDouble / denom
      }
    }
    PTgivenH
  }




  //==================================================

  def learn() {
    val m = numTests
    val n = numHistories

    //construct matrices
    /*PH    : n x 1
     *PTH   : m x n
    *PTaoH : m x n (for each ao) 
    */
    val PH = computePH()
    val PTH = computePTH()

    /*Find the U matrix using svd
    * U: m x k
    * 
    * because svd yields
    * PTH = U * S * V.t => (m x n) = (m x m) (m x n) (n x n) 
    * but we only use the first k column of U (corresponding to highest k singular values)
    */
    require(PTH.numRows == m && PTH.numCols == n)
    val s = svd(PTH)
    var Ufull = s._1
    require(Ufull.numRows == m && Ufull.numCols == m)
    //make U matrix by choosing top singular values unless rank=-1 in which case use the whole U matrix

//    println("estimated rank = "+k)    

    //==================================================
    // Compute parameters for tpsr
    if(tpsr){
      import srltk.tools.linearalgebra.LinearAlgebra._
      val est1 = estRankSVD(s._2,this.sigmThresh)
      val k = if(est1 < maxRank) est1 else maxRank
      this.estimatedRank = k
      val U = DenseMatrix.zeros[Double](Ufull.numRows, k)
      U := Ufull(0 until Ufull.numRows, 0 until U.numCols)
      require(U.numRows == m && U.numCols == k)

      /* b*: k x 1
       * U.t * PTH * 1k == (k x m) * (m x n) * (n x 1) = k x 1 */
      mStar = U.t * PTH * DenseVector.ones[Double](PTH.numCols)
      require(mStar.length == k)
      /* b_inf: k x 1
       * (PTH.t * U)^-1 * PH = ((n x m) (m x k)) ^-1 * (n x 1) = (k x n) (n x 1) = k x 1 */
      val PTHU: DenseMatrix[Double] = PTH.t * U //n x m * m x k = n x k
      require(PTHU.numRows == n && PTHU.numCols == k)

      // k x n		
      val invPTHU: scalala.tensor.mutable.Matrix[Double] = pinv(PTHU) // k x n		
      require(invPTHU.numRows == k && invPTHU.numCols == n)
      mInfinity = invPTHU * PH
      require(mInfinity.length == k)

      /* Mao (for each ao) is kxk */
      Mao = new Array[DenseMatrix[Double]](numActions * numObservations)
      for (a <- 0 until numActions; o <- 0 until numObservations) {
        val ind = a * numObservations + o
        val phaotU = DenseMatrix.zeros[Double](k,n)
        phaotU := (db.computePHaoT(a,o) * U).t //((n x m) * (m x k))' = (n x k)' = (k x n)
        Mao(ind) = phaotU * invPTHU.t //(k x n) (n x k) = k x k        
      }
    }
    //==================================================
    // Compute parameters for psr
    else {
      //find core tests and core histories
      import srltk.tools.linearalgebra.LinearAlgebra._
      import srltk.tools.linearalgebra.StrongRRQR

      val k =  maxRank
      this.estimatedRank = k
      val pth = computePTgivenH()
      val m = findBasis(pth,k,-1)
      val Z =  m._1
      val QT = m._2.toList
      val QH = m._3.toList      


/*      println("pvt1 hists: ")
      for(i <- pvt(0 until k)){
        for(j <- db.histories(i))
          print(j._1+", "+j._2+" ")
        println()
      }
      println()
      println("pvt2 hists: ")
      for(i <- pvt2(0 until k)){
        for(j <- db.tests(i))
          print(j._1+","+j._2+"  ")
        println()
      }
      println()

//      println(PQT)
      println()
      println("pvt1 = "+pvt(0 until k).asRow)
      println("pvt2 = "+pvt2(0 until k).asRow)*/
//      println("rank should be "+k)
      require(Z.numRows == k && Z.numCols == k)
//      require(rank(Z) == k)

      val invZ = pinv(Z)

      mStar = Z * toDense(computePH()(QH)).asCol
      /* mInf^T = I_(1xk) * Z^-1
       * */
      mInfinity = DenseVector.zeros[Double](k)
      for(i <- 0 until k)
        mInfinity(i) = invZ(0 until k, i).sum
      
      Mao = new Array[DenseMatrix[Double]](numActions * numObservations)
      for (a <- 0 until numActions; o <- 0 until numObservations) {
        val ind = a * numObservations + o
        val Qao = db.computePaoTgivenH(a,o)(QT,QH)
        Mao(ind) = invZ * Qao
      }
    }
  }


  //======================================================================
  //FOR TESTING:


  def jointPTHSum() : Double = {
    val PTH = computePTH()
    val PT = computePT()
    var sum = 0d
    for(t <- 0 until numTests;
        h <- 0 until numHistories)
      sum += PTH(t,h) * PT(t)
    sum
  }

  //==================================================

  def computePaT(): Array[DenseVector[Double]] = {
    val PaT = new Array[DenseVector[Double]](numActions)
    for (a <- 0 until numActions){
      PaT(a) = db.computePaT(a)
    }
    PaT
  }

  def computePTaoH(): Array[DenseMatrix[Double]] = {
    val PTaoH = new Array[DenseMatrix[Double]](numActions * numObservations)
    for (a <- 0 until numActions;
         o <- 0 until numObservations){
      PTaoH(a*numObservations+o) = DenseMatrix.zeros[Double](numTests,numHistories)
      PTaoH(a*numObservations+o) := db.computePHaoT(a,o).t
    }
    PTaoH
  }

  //ensure sum of ptaoh is 1
  def jointPTaoHSum(): Double = {
    val PTaoH = computePTaoH()
    val PaT = computePaT()
    var sum = 0d
    for(t <- 0 until db.tests.length;
        a <- 0 until db.na;
        o <- 0 until db.no){
      sum += PTaoH(a*db.no+o)(t,0 until db.histories.length).sum * PaT(a)(t)
    }
    sum
  }



}
