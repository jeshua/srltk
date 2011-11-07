package srltk.vis.spacesconnect
import srltk.common._;

class SpacesActionInterface(val index: Int, val numActions: Int, val a: IntAction)
  extends spaces.framework.util.action.DiscreteAction {
  def this(action: IntAction, numActions : Int) = this(action.n, numActions, action)

  def getValue(): Int = index
  def getValues(): Array[spaces.framework.util.action.DiscreteAction] =
    {
      val t: IndexedSeq[spaces.framework.util.action.DiscreteAction] =
        for (i <- 0 until numActions) yield new SpacesActionInterface(i, numActions, a)
      t.toArray
    }

  def getAction(i: Int): SpacesActionInterface = new SpacesActionInterface(i, numActions, a)
  def srltkAction() = IntAction(index);
  override def toString() = srltkAction.toString
}
