/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
 * 
 * Copyright 2011 jeshua
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package srltk.tools.features

import srltk.api.agent.FeatureExtractor
import scala.collection.mutable.ArraySeq
import scala.util.Random
import scalala.tensor.Vector
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.tensor._
import scala.collection.mutable.HashMap

//ranges: min,max for each dimension so we can normalize input
//bins: tile granularity for each dimension
//numGrids: number of grids to place
class CMAC(
  val ranges: List[(Double, Double)],
  val bins: List[Int],
  val numGrids: Int,
  val rng: Random) extends FeatureExtractor {

  val dimensions = ranges.length
  val tilesPerGrid = bins.reduceLeft(_ * _)
  val numFeatures = tilesPerGrid * numGrids

  //the size of each tile is extents of dimension over grid resolution
  val binWidth = for (i <- 0 until dimensions)
    yield (ranges(i)._2 - ranges(i)._1) / (bins(i) - 1);

  //grid shifts
  val offsets = for (i <- 0 until numGrids; j <- 0 until dimensions)
    yield rng.nextDouble();

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
  private def getTile(input: Vector[Double], grid: Int): Int = //Vector[Int] =
    {
      def accumulate(i: Int = 0, mult: Int = 1): Int = {
        if (i >= dimensions) 0
        else mult * getTile(input(i), i, grid) + accumulate(i + 1, mult * bins(i))
      }
      accumulate()
    }

  def getTilingIndices(input: Vector[Double]): Vector[Int] =
    {
      if (input.length != dimensions) throw new IllegalArgumentException("Input should be " + dimensions + " but is " + input.length)
      var ret: mutable.Vector[Int] = DenseVector.zeros[Int](numGrids)

      for (i <- 0 until numGrids) {
        val v = getTile(input, i) + i * tilesPerGrid
        ret(i) = v
      }
      ret
    }

  var invocations = 0
  //return the tiles for a data point for all grids
  def getTiling(input: VectorCol[Double]): VectorCol[Double] =
  {
    invocations+=1
    val indices = getTilingIndices(input)
    var ret: mutable.Vector[Double] = SparseVector.zeros[Double](numFeatures)
    for (v <- indices) ret(v) = 1
    ret.asInstanceOf[VectorCol[Double]]
  }
  
  
  //==================================================
  override def extract(input: VectorCol[Double]): VectorCol[Double] = getTiling(input)
  override def length = numFeatures
  override val isSparse = true

}
