package srltk.common

object Implicits {
	implicit def intToAction(n : Int) : IntAction = IntAction(n)
	implicit def actionToInt(a : IntAction) : Int = a.n	
}