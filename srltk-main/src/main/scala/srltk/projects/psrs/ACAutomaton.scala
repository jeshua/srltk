package srltk.projects.psrs
import collection.mutable.Queue
import srltk.projects.psrs.pomdps._
import scala.collection.immutable.TreeMap
import srltk.api.agent._
import srltk.api.domain._
import java.io.FileOutputStream
import java.io.PrintStream
import java.net.URL
import javax.imageio.ImageIO
import javax.swing.JLabel
import javax.swing.JFrame
import javax.swing.ImageIcon
import java.io.BufferedReader
import java.io.InputStreamReader
import javax.swing._


object ACAutomaton {

  def main(args : Array[String]) = {
    val pomdp = new Tiger
    val na = pomdp.numActions
    val no = pomdp.numObservations
    val numSamples = 50000
    val rng = new scala.util.Random()
    val samples = pomdp.generateSamples(numSamples,rng)
    val t = Discover(4,7,samples,rng)
    val alphabet = for(i <- 0 until na;j <- 0 until no) yield (i,j).toString
    def sampleToInt(s : (Int,Int)) = (s._1 * no + s._2)
    def seqToIntArray(seq : Array[(Int,Int)]) : Array[Int] = {
      for(s <- seq) yield sampleToInt(s)
    }
    def seqsToIntArrays(seqs : Array[Array[(Int,Int)]]) : Array[Array[Int]] = {
      for(seq <- seqs) yield for(s <- seq) yield sampleToInt(s)
    }


    val db = new DiscreteSampleDatabaseAC(pomdp.numActions, 
                                          pomdp.numObservations, 
                                          t,t)
    db.append(samples)
    printf("done\n")
    System.out.flush()

    //val autom = new ACAutomaton(seqsToIntArrays(t._1),no*na)
    //autom.display()
  }
}

class ACAutomaton(
  input : Array[Array[Int]],
  alphabetSize : Int
) 
{
  val HEAD = DATrie.HEAD
  val EMPTY = -1
  def offset = HEAD+1
  val seqs : Array[Array[Int]] = for(i <- input) yield for(j <- i) yield j+offset
  val sigma : Array[Int] = (for(i <- 0 until alphabetSize) yield i+offset).toArray
  

  //first build up the trie
  val trie = new DATrieMem(offset,alphabetSize+offset)
  
  //insert seqs into trie
  for(t <- 0 until seqs.length){
    trie.insert(seqs(t),t)
  }
  
  //failure function f(state) = state'
  val failure = new Array[Int](trie.base.length)
  //output function out(state) = {output addresses}
  val output = new Array[List[Int]](trie.base.length)
  for(i <- 0 until output.length) output(i) = Nil
  for(t <- 0 until seqs.length){
    val state = trie.find(seqs(t))
    output(state) ::= t
  }
  //goto function
  def goto(state : Int, a : Int) : Int = {
    val child = trie.getChild(state,a)
    if(child > HEAD) child
    else if(state == HEAD) HEAD
    else EMPTY
  }

  //set initial failure function and enqueue first level states
  val Q = new Queue[Int]
  for(a <- sigma){
    var q : Int = goto(HEAD,a)
    if(q != HEAD){
      failure(q) = HEAD
      Q.enqueue(q)
    }
  }
  
  //now compute all failure and output functions with BFS
  while(Q.size > 0){
    val r = Q.dequeue()
    for(a <- sigma){
      val u = goto(r,a)
      if(u != EMPTY){
        Q.enqueue(u)
        //follow failures back to get longest proper suffix
        var v = failure(r)
        while(goto(v,a)==EMPTY && v!=HEAD) { v = failure(v);}
        failure(u) = goto(v,a)
        output(u) ++= output(failure(u))
      }
    }
  }

  //==================================================
  var currentState = HEAD
  def reset() = currentState = HEAD
  
  def step(input : Int) : Array[Int] = {
    val a = input+offset
    while(goto(currentState,a)==EMPTY && currentState != HEAD){
      currentState = failure(currentState)
    }
    currentState = goto(currentState,a)
    output(currentState).toArray    
  }

  //==================================================
  def display() =
    {
      var digraph = ""
      digraph += "digraph ACAutomaton {\n"
      digraph += "\trankdir=LR;\n"
      digraph += "\tsize=\"8,5\";\n"
      digraph += "\tnode [shape=circle];\n"

      val queue = new Queue[Int]
      queue.enqueue(HEAD)
      while(queue.size > 0){      
        val state = queue.dequeue()
        val out = "#"+state+": "+(for(i <- output(state)) yield seqs(i).toList.toString+"="+i).toList.toString
        digraph += "\t\""+state++"\"[label=\""+out+"\"]\n";
        for(a <- sigma) {
          val child = trie.getChild(state,a)
          if(child > HEAD) {
            digraph += "\t\""+state++"\" -> \""+child+"\"[label=\""+a+"\"]\n";
            println(child)
            queue.enqueue(child)
          }    
        }
        digraph += "\t\""+state++"\" -> \""+failure(state)+"\"[style=dashed]\n";
      }
      digraph += "}\n"
      System.out.println(digraph);
      showDot(digraph)
    }

  def showDot(graph : String,outputFile : String = null) = {
    val window = new JFrame
    val filename = System.getProperty("java.io.tmpdir") + "/displayac.dot"
    val jpgFilename = if(outputFile == null) System.getProperty("java.io.tmpdir") + "/displayac.jpg" else outputFile
    val output = new FileOutputStream(filename)
    val file = new PrintStream(output)
    file.println(graph)
    file.close()
    val p = Runtime.getRuntime().exec("dot -Tjpg -Gdpi=212 -o"+ jpgFilename +" "+ filename)
    val input =new BufferedReader(new InputStreamReader(p.getErrorStream));
    var line : String = "" 
    while( {line = input.readLine;  line!= null} ) { println(line) }
    val url = new URL("file://"+jpgFilename)
    val image = ImageIO.read(url)
    val label = new JLabel(new ImageIcon(image))
    val f = new JPanel()
    f.add(label)
    val scrollPane = new JScrollPane(f)
    window.add(scrollPane)
    window.setSize(800,800)
    window.setVisible(true)

    
    f
  }  



}
