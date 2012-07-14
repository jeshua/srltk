package srltk.common
class DomainDescription(
  val num_actions : Int,
  val obs_dim : Int){}


trait Domain[Obs <: Observation, Act <: Action]{
  def act(a : Act) : Obs
}

