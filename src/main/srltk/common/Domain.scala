package srltk.common


class DomainDescription(
    val numActions : Int,
    val obsDim : Int = -1,
    val numObservations : Int = -1
) 


trait Domain{
  val domainDescription : DomainDescription
  def act(a : Action) : Observation
}

