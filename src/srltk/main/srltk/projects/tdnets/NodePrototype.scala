/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package srltk.projects.tdnets
import scalala.tensor._
import srltk.api.agent.FeatureTransform
import srltk.api.agent.IdentityTransform
import srltk.tools.features.CMAC
import srltk.api.domain.Observation
import srltk.api.domain.Action
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector
import scala.collection.mutable._
import srltk.projects._

trait NodePrototype{
  def name : String
  def getSize : (Int)=>Int
  def generate (obsDim : Int, numA: Int) : Node		 
  def flattenPrototypes: List[NodePrototype] = List(this)
  def flattenPrototypesRecursive(n: HashSet[NodePrototype]): List[NodePrototype] = 
	  if(!n.contains(this)) {n.add(this);List(this)} else Nil
}

/**
 * A node prototype defines a node's connections and dependencies.
 *
 * Question Network:
 *   1q) targetNodes: list of nodes who's predictions (at the next timestep)
 *       are what this node is attempting to predict
 *   2q) [targetFunction: a function that turns the target's predictions
 *       into a single vector.]
 *         *by default this function stacks all target prediction
 *   3q) [questionRegressor: learns a function from targets to questions]
 *         *by default just returns vector targetFunction's output to question vector
 * Answer Network:
 *   1a) inputNodes: list of nodes who's predictions are used as input
 *   2a) [inputFunction: a function that turns the list of input inputs into
 *       a single vector.]
 *         *by default this stacks all inputs into into vector.
 *   3a) answerRegressor : learns a function from inputs to answers
 *
 */

class InternalNodePrototype(_name: String = "unnamed", _getSize: (Int) => Int = null) 
extends NodePrototype {
  def name = _name
  def getSize = _getSize
  
  //Description in comment above class definition
  //(1q)
  var targetPrototypes: List[NodePrototype] = Nil
  //(2q)
  var targetFunction: MultiFeatureTransform = new StackTransform
  //(3q)
  var questionRegressor: (Int, Int) => Predictor = (a: Int, b: Int) => null
  //(1a)
  var inputPrototypes: List[NodePrototype] = Nil
  //(2a)
  var inputFunction: MultiFeatureTransform = new StackTransform
  //(3a)
  var answerRegressor: (Int, Int) => Predictor = null
  //optionally set the error function
  var error: ErrorFunction = SimpleError //error function associated with prediction
  
  /**
   * Generate: constructs a network starting from this node prototype.
   *   Notice that this means creating all input nodes and target nodes
   *   as needed. We ensure nodes are not created in duplicate through
   *   the _directory hash map which is passed to recursive calls of
   *   generate
   */
  def generate(obsDim : Int, numA : Int) = generate(obsDim,numA,null,None)
  def generate(observationDimension: Int, numActions: Int, network : List[NodePrototype],
    _directory: Option[Map[NodePrototype, Node]] //optionally provide a node directory so as to not recreate nodes
    ): Node =
    {
      //create a directory if one was not passed in
      val d: Map[NodePrototype, Node] =
        _directory match {
          case Some(directory) => directory
          case None => new HashMap[NodePrototype, Node]
        };

      //if this node matcher is in the directory, just return the associated node
      if (d.contains(this)) d(this)
      //otherwise, create the network
      else {
        //require that we have an answer regressor
        if (answerRegressor == null)
          throw new IllegalArgumentException("Error generating node, answer regressor is undefined.")

        //make sure target list isn't empty
        if (targetPrototypes == Nil)
          targetPrototypes ::= LeafNodes.ObservationNode;
      
        //make sure input list isn't empty
        if (inputPrototypes == Nil)
          inputPrototypes ::= LeafNodes.ObservationNode
          
        val targetNodes = new Array[Node](targetPrototypes.length)
        val inputNodes = new Array[Node](inputPrototypes.length)
        val outDim = inferOutputDimension(observationDimension)
        val inDim = inferInputDimension(observationDimension)


        //create a node from this prototype
        val me = new InternalNode(
          _name,
          targetNodes,
          targetFunction,
          questionRegressor(inDim, outDim),
          inputNodes,
          inputFunction,
          answerRegressor(inDim, outDim),
          error,
          inDim,
          outDim)

        //important: add this node to the directory before creating children
        //  because children may reference this node prototype. This ensures only one
        //  node is created for each prototype.
        d(this) = me

        //generate all the target nodes
        for (i <- 0 until targetPrototypes.length)
          targetNodes(i) = 
            targetPrototypes(i) match {
            case i : InternalNodePrototype => i.generate(observationDimension, numActions, network, Some(d))
            case i : LeafNode  => i.generate(observationDimension, numActions)
        }

        for (i <- 0 until inputPrototypes.length)
          inputNodes(i) = inputPrototypes(i) match {
            case i : InternalNodePrototype => i.generate(observationDimension, numActions, network, Some(d))
            case i : LeafNode  => i.generate(observationDimension, numActions)
        }

        me
      }
    }

  //======================================================================

  
  //add single target predictor
  def predicts(target: NodePrototype): InternalNodePrototype = {
    if (getSize == None && target == this)
      throw new IllegalArgumentException("Cannot predict self without explicit output size")
    targetPrototypes ::= target
    this
  }
  //add list of predictors 
  def predicts(targets: List[NodePrototype]): InternalNodePrototype = {
    for (i <- targets) predicts(i)
    this
  }
  def predicts(targets: List[NodePrototype],
    targetFunction: MultiFeatureTransform
    ): InternalNodePrototype =
    {
      predicts(targets)
      this.targetFunction = targetFunction
      this
    }

  def predicts(targets: List[NodePrototype], targetFunction : (Array[Feats]) => Feats) 
    : InternalNodePrototype =
    {
      predicts(targets,new MultiFeatureTransformWrapper(targetFunction))
      this
    }
  
    def predicts(target: NodePrototype, targetFunction: MultiFeatureTransform) :
    	InternalNodePrototype = predicts(List(target),targetFunction)
    def predicts(target: NodePrototype, targetFunction: (Array[Feats])=>Feats) :
    	InternalNodePrototype = predicts(List(target),targetFunction)
    
  //========================================
  //adding inputs
    
  def observes(input: NodePrototype) = {
    this.inputPrototypes ::= input
    this
  }
  def observes(input: List[NodePrototype]) = {
    this.inputPrototypes :::= input
    this
  }  
  def observes(input: NodePrototype, targetFunction : (Array[Feats]) => Feats): InternalNodePrototype =
    {
      observes(input)
      this.targetFunction = new MultiFeatureTransformWrapper(targetFunction)
      this
    }
  
  def observes(input: NodePrototype, ex: FeatureTransform) = {
    this.inputPrototypes ::= input
    this.inputFunction = new MultiTransform(ex)
    this
  }

  
  //========================================
  //adding other
  def withAnswerPredictor(p: (Int, Int) => Predictor = null) =
  { this.answerRegressor = p; this }
  def withError(error: ErrorFunction) { this.error = error; this }
  def withQuestionPredictor(p: (Int, Int) => Predictor = null) =
  { this.questionRegressor = p; this }

  //======================================================================

  override def flattenPrototypes: List[NodePrototype] = {
    val n = new HashSet[NodePrototype]
    flattenPrototypesRecursive(n)
  }
  override def flattenPrototypesRecursive(n: HashSet[NodePrototype]): List[NodePrototype] = {
    if (n.contains(this)) Nil
    else {
      n.add(this.asInstanceOf[NodePrototype])
      val t = (for (target <- targetPrototypes) yield target.flattenPrototypesRecursive(n)).flatten.toList
      val c = (for (input <- inputPrototypes) yield input.flattenPrototypesRecursive(n)).flatten.toList
      val ret = this :: (t ::: c)

      ret
    }
  }
  //======================================================================
  //dimension inference

  def inferOutputDimension(observationDimension: Int): Int = { resolveOutputDim(observationDimension)(this) }

  def inferInputDimension(observationDimension: Int): Int = {
    val outDims = resolveOutputDim(observationDimension)
    val inputs = (for (n <- inputPrototypes) yield SparseVector.zeros[Double](outDims(n)))
    val size = inputFunction(inputs.toArray).length
    size
  }

  def resolveOutputDim(observationDimension: Int): HashMap[NodePrototype, Int] = {
    val outDims = new HashMap[NodePrototype, Int]
    var nodes: List[NodePrototype] = this.flattenPrototypes
    //fill in output size for all those with explicitly specified size functions
    for (n <- nodes) if (n.getSize != null) outDims(n) = n.getSize(observationDimension)
    //now sweep over remaining nodes
    while (outDims.size < nodes.length) {
      //do one sweep over all nodes
      var resolvedAny = false
      for (_node <- nodes) {        
        //try to infer output dimension from all target nodes
        if (!outDims.contains(_node)) {
          val node = _node.asInstanceOf[InternalNodePrototype]
          //check if all targets are resolved
          val targetsResolved = node.targetPrototypes.foldLeft(true)(_ && outDims.contains(_))
          def d(np: NodePrototype): Int = (if (outDims.contains(np)) 1 else 0)
          val numResolved = node.targetPrototypes.foldLeft(0)(_ + d(_))
          if (targetsResolved) {
            val targets = for (target <- node.targetPrototypes) yield SparseVector.zeros[Double](outDims(target))
            //now pass it through the targetfunction to get the size
            outDims(node) = node.targetFunction(targets.toArray).length
            resolvedAny = true
          } 
        }
      }
      if (!resolvedAny) throw new IllegalArgumentException("Cannot resolve prediction dimension for network")
    }
    outDims
  }
}
//======================================================================

