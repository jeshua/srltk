package srltk.projects.psrs

class LimitedHistoryArray[A : Manifest](maxLength : Int, defaultVal : A) {
  val ar = new Array[A](maxLength)
  for(i <- 0 until ar.length) ar(i) = defaultVal
  var head = 0
  //we have access to elements [maxInd-maxLength,...,maxInd]
  var maxInd = 0

  def checkBounds(ind : Int) : Unit = {
    if(ind > maxInd || maxInd-ind >= maxLength){
      val er = "Index out of bounds: "+ind+" requested but bounds are "+(1+maxInd-maxLength)+" to "+maxInd
      throw new IndexOutOfBoundsException(er)
    }
  }
  //convert requested index to pointer location in underlying array

  private def boundPointer(pointer : Int) = {
    val d = pointer % maxLength
    if(d < 0) d + maxLength
    else d
    
  }
  private def indexToPointer(ind : Int) : Int = {
    boundPointer(head - (maxInd-ind))
  }
  private def pointerToIndex(pointer : Int) : Int = {
    //head == maxInd
    
    val distance = 
      if(pointer > head)
        head + maxLength-pointer
      else
        head-pointer
    maxInd-distance
  }


  def update(ind : Int, e : A) = {
    //simple case, just return an element
    if(ind <= maxInd){
      checkBounds(ind)
      ar(indexToPointer(ind)) = e
    } else {
      for(i <- maxInd+1 to ind){
        head = boundPointer(head+1)
        ar(head) = defaultVal
      }
      maxInd = ind
      ar(indexToPointer(ind)) = e
    }
  }

  def apply(ind : Int) : A = {
    checkBounds(ind)
    ar(indexToPointer(ind))
  }
}
