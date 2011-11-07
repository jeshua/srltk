/**
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 * **/

package srltk.common
trait Domain{
  val domainDescription : DomainDescription
  def act(a : Action) : Observation
}

