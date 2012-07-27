package srltk.common


class Action
case class IntAction(val n: Int) extends Action

trait State[T <: State[T]]{
  def copy() :T;
}

//==================================================
//DOMAIN and DOMAIN-INTERFACE

trait DomainAgentInterface[St <: State[St], Obs <: Observation, Act <: Action]{
  def numActions() : Option[Int]
  def obsDim() : Option[Int]  
  def stateToObs(st : St) : Obs  
}

trait Domain[St <: State[St], Act <: Action]{
  def getState() : St
  def act(a : Act)  
}

//==================================================
//OBSERVATION TYPES

abstract class Observation
(val reward : Double = 0d)
{}
