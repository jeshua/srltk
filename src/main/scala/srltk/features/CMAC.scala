package srltk.features
import srltk.common._
import scala.collection.mutable.ArraySeq
import scala.util.Random
import scala.collection.mutable.HashMap
import scalala.tensor.dense.DenseVector
import scalala.tensor.VectorCol
import scalala.tensor.sparse.SparseVector

//ranges: min,max for each dimension so we can normalize input
//bins: tile granularity for each dimension
//numGrids: number of grids to place
class CMAC(
  val ranges: List[(Double, Double)],
  val bins: List[Int],
  val numGrids: Int) extends FeatureExtractor {

  val dimensions = ranges.length
  val tilesPerGrid = bins.reduceLeft(_ * _)
  val numFeatures = tilesPerGrid * numGrids

  //the size of each tile is extents of dimension over grid resolution
  val binWidth = for (i <- 0 until dimensions)
    yield (ranges(i)._2 - ranges(i)._1) / (bins(i) - 1);

  //grid shifts
  val offsets = for (i <- 0 until numGrids; j <- 0 until dimensions)
    yield GlobalRNG.nextDouble();

  private def normalize(value: Double, dim: Int) = (value - ranges(dim)._1) / binWidth(dim)

  //get the tile for one value in one dimension in one grid
  //output will be in set {0,1,...,bins[dim]-1}
  private def getTile(value: Double, dim: Int, grid: Int): Int = {
    //shift by offset and multiply by bin size
    val t = scala.math.floor(normalize(value, dim) +
      offsets(grid * dimensions + dim)).toInt;
    //make sure return is in range
    if (t >= bins(dim)) bins(dim) - 1
    else if (t < 0) 0
    else t
  }

  //return the tiles for a data point for one grid for one grid
  private def getTile(input: VectorCol[Double], grid: Int): Int = //Vector[Int] =
    {
      def accumulate(i: Int = 0, mult: Int = 1): Int = {
        if (i >= dimensions) 0
        else mult * getTile(input(i), i, grid) + accumulate(i + 1, mult * bins(i))
      }
      accumulate()
    }

  def getTilingIndices(input: VectorCol[Double]): Seq[Int] =
    {
      if (input.length != dimensions) throw new IllegalArgumentException("Input should be " + dimensions + " but is " + input.length)      
      for (i <- 0 until numGrids) 
        yield getTile(input, i) + i * tilesPerGrid        
    }

  var invocations = 0
  //return the tiles for a data point for all grids
  def getTiling(input: VectorCol[Double]): VectorCol[Double] =
  {
    invocations+=1
    val indices = getTilingIndices(input)
    var ret = SparseVector.zeros[Double](numFeatures)
    for (v <- indices) ret(v) = 1
    ret.asInstanceOf[VectorCol[Double]]
  }
  
  
  //==================================================
  override def apply(input: VectorCol[Double]): VectorCol[Double] = getTiling(input)
  override def length = numFeatures
  override val isSparse = true

}
