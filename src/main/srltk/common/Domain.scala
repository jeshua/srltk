package srltk.common
trait Domain{
  val domainDescription : DomainDescription
  def act(a : Action) : Observation
}

