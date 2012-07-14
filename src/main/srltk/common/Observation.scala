package srltk.common
import scala.collection._
import scala.collection.generic._

abstract class Observation
(val reward : Double = 0d)
{}

abstract class FeatureObservation
(val features : Feats) extends Observation
{}

abstract class IndexObservation
(val n : Int)
{}