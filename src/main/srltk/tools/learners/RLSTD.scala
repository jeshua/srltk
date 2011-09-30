/**
 * *****************************************************************************
 * Scala Reinforcement Learning Toolkit
 *  @author Jeshua Bratman
 *  @email jeshuabratman@gmail.com
 *
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
 * ****************************************************************************
 */
package srltk.tools.learners

import srltk.api.domain._
import scalala.scalar._
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;
import srltk.api.agent._

class RLSTD(val gamma: Double)
  extends LearnerV {

  //mutable state
  var d: Int = 0
  var theta: VectorCol[Double] = null
  var C : Matrix[Double] = null
  var g : SparseVector[Double] = null
  
  
  //imprint learner with example observation and action
  override def onImprint() {
    //initialize theta with size of imprinted observation
    d = imprintedO().features.length
        
    //theta = DenseMatrix.zeros[Double](d,d)
    theta = DenseVector.zeros(d)
    C = DenseMatrix.eye[Double](d,d) * .05
    g = scalala.tensor.sparse.SparseVector.zeros(d)
    
  }
  
  
  def value(o : Observation) : Double = theta.t * o.features

  //update value function from observed transition
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation) = learn(otm1, ot)
  override def learn(o1: Observation, a1: Action, o2: Observation) = learn(o1, o2)
  def learn(o1: Observation, o2: Observation) {
	  val f = o1.features
	  val fp = o2.features
	  //@TODO: this computation can be done more efficiently for sparse vectors
	  
	  val g = (f - gamma :* fp).t * C  // 1 x d * d x d = 1 x d
	  //val diff = (f - gamma :* fp)//this will be sparse if f is sparse
	  /*var diff : Vector[Double] = SparseVector.zeros[Double](d)
	  f.foreachNonZeroPair((i: Int, v1: Double) => {
		  diff(i) += v1
	  })	  
	  fp.foreachNonZeroPair((i: Int, v2: Double) => {
		  diff(i) -= gamma * v2
	  })	  
	  
	  g = scalala.tensor.sparse.SparseVector.zeros(d)
	  diff.foreachNonZeroPair((i: Int, v: Double) => {
	    
	    for(j <- 0 until d)
	    	g(i) += v :* C(i,j)
	  })*/
	  
	  //var a : Double = 1 + g.t*f //1 x d * d x 1 = 1x1
	  //SPARSE
	  var a = 1.0
	  f.foreachNonZeroPair((i: Int, v: Double) => {
		  a += g(i) * v
	  })
	  
	  val v : Vector[Double]= f.t * C //1xd * dxd = 1xdt
	  val delta = value(o2) :* gamma - value(o1) :+ o2.reward
	  //theta += delta :/ (v :* a)
	  println(v:*a)
	  //C -= (g.t*v) :/ a
	  g.foreachNonZeroPair((i: Int, val1: Double) => {
	    v.foreachNonZeroPair((j: Int, val2: Double) => {
	    	C(i,j) -= val1 * val2 / a
	    })
	  })
  }
}
