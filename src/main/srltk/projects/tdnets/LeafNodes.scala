package srltk.projects.tdnets
import srltk.api.domain.Observation
import srltk.projects._
abstract class LeafNode extends Node with NodePrototype{
	def getYTilde(obs : Observation) = getY(obs)
	def generate(obsDim : Int, numA : Int) : Node 
}

class _ObservationNode extends LeafNode {
	def name = "ObservationNode"
  def getY(obs : Observation) = obs.features
  override def getSize = (observationDimension : Int) => observationDimension
  def generate(obsDim : Int, numA : Int) : Node  = LeafNodes.ObservationNode
}
class _RewardNode extends LeafNode {
  def name = "RewardNode"
  def getY(obs : Observation) = toFeats(obs.reward)
  override def getSize = (observationDimension : Int) => 1
  def generate(obsDim : Int, numA : Int) : Node  = LeafNodes.RewardNode  
}

package object LeafNodes{
  val RewardNode : LeafNode = new _RewardNode
  val ObservationNode : LeafNode = new _ObservationNode
}