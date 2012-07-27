package srltk.driver
import srltk.common._
import scala.collection.mutable.ListBuffer

class ManagedHistory[Obs <: Observation, Act <: Action](val maxLength: Int) {
  require(maxLength > 0)

  val observations = new ListBuffer[Obs];
  val actions = new ListBuffer[Act];
  def t = observations.length
  //number of observations in the observation buffer
  def length(): Int = observations.length

  //append observations and actions
  def append(o: Obs): Unit = {
    //can't append nth observation if there aren't n-1 actions
    require(observations.length == actions.length)

    observations.append(o)
    if (observations.length > maxLength) {
      observations.remove(0)
      actions.remove(0)
    }
  }
  def append(a: Act): Unit = {
    //must always have 1 more observation than action
    require(observations.length - 1 == actions.length)
    actions.append(a)
  }

  def clear(): Unit = { observations.clear; actions.clear(); }

  //o_t-n
  def o_t(n: Int = 0): Obs = {
    require(n <= 0)
    observations(length + n - 1)
  }
  //a_t-n
  def a_t(n: Int = 0): Act = {
    require(n <= 0)
    val aInd = length + n - 1
    actions(aInd)
  }
}
