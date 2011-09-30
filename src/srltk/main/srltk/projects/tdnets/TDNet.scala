
package srltk.projects.tdnets

import javax.swing.JTabbedPane
import javax.swing.JPanel
import srltk.projects._
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
import scala.collection.mutable._

import srltk.vis.ActivePlot
import java.awt.Color
import scala.collection.SortedMap
import scala.collection.mutable.HashMap

class TDNet(nodeList : List[InternalNodePrototype], observationDimension : Int, numActions : Int = 1){
  def this(root : InternalNodePrototype, observationDimension : Int) = this(List(root),observationDimension)
  
  val internalNodes = TDNet.getInternalNodes(nodeList, observationDimension, numActions)
  val allNodes = TDNet.getNodes(nodeList, observationDimension, numActions)
  val size = allNodes.size
  var ot : Observation = null
  var otp1 : Observation = null
  var lastErrors: Map[String,Double] = new HashMap[String,Double]
  
  def learn(f : Double) : Unit = learn(new Observation(toFeats(f)),null)
  def learn(f : Feats) : Unit = learn(new Observation(f),null)
  def learn(o : Observation) : Unit = learn(o,null)
  def learn(o : Observation, a : Action) : Unit = {
    //allow all nodes to observe on first loop
    var newStates = new HashMap[String,NodeState]
    for(n <- internalNodes) 
    	newStates(n._1) = n._2.observe(o,a)
    //and perform update on second loop
    for(n <- internalNodes){ 
    	n._2.state = newStates(n._1)
    	n._2.update()       
    	lastErrors(n._1) = n._2.lastError 
    }
    if(otp1 != null && ot!=null)
    	updateMonitor()
    otp1 = o
    ot = otp1
  }
  var updateMonitor : () => Unit = () => ()
  
  def showDot(outputFile : String = null) =
    {
      val filename = System.getProperty("java.io.tmpdir") + "/tdnet.dot"
      val jpgFilename = if(outputFile == null) System.getProperty("java.io.tmpdir") + "/tdnet.jpg" else outputFile
      val output = new FileOutputStream(filename)
      val file = new PrintStream(output);

      file.println("digraph TDNET {");
      file.println("\trankdir=LR;");
      file.println("\tsize=\"8,5\";");
      file.println("\tnode [shape=circle];");
      for (n <- allNodes) {
        val node = n._2
        val id = node.hashCode
        val name = node.name
        if(node.isInstanceOf[LeafNode]){
            file.println("\tLR_" + id + " [label=\"" + name + "\" rank=max];")
            if(node eq LeafNodes.ObservationNode){
              for(n <- allNodes; if(!n._2.isInstanceOf[LeafNode]))
              {
                file.println("\tLR_" + id
                        + " -> LR_" + n._2.hashCode + "  [ penwidth=1 ];")
              }                        
            }
        }
        else if(node.isInstanceOf[InternalNode])
        {
          val inNode = node.asInstanceOf[InternalNode]
            file.println("\tLR_" + id + " [label=\"" + name + "\" ranke=min];")
            for (input <- inNode.inputNodes; if(!input.isInstanceOf[LeafNode])) 
            {
              val pid = input.hashCode
              file.println("\tLR_" + pid
                  + " -> LR_" + id + "  [ penwidth=1 ];")
            }
            for (target <- inNode.targetNodes) {
                val cid = target.hashCode
                  file.println("\tLR_" + id
                      + " -> LR_" + cid + " [ penwidth=5 ];");
                //val txt = "\tLR_" + id + " -> LR_" + cid + " [ penwidth=5,label=\"a1,a2,...\",arrowhead=\"odiamond\" ];"
            }
        }
      }
      file.println("}");
      file.close();
      
      val p = Runtime.getRuntime().exec("dot -Tjpg -o"+ jpgFilename +" "+ filename)
      //val p = Runtime.getRuntime().exec("ls -l")
      //val input =new BufferedReader(new InputStreamReader(p.getInputStream()));
      val input =new BufferedReader(new InputStreamReader(p.getErrorStream));
      var line : String = "" 
      while( {line = input.readLine;  line!= null} ) { println(line) }
      val url = new URL("file://"+jpgFilename)
      val image = ImageIO.read(url)
      val label = new JLabel(new ImageIcon(image))
      val f = new JPanel()
      f.add(label)
      f.setLocation(200,200)
      f.setVisible(true)
      f
    }  



  //============================================================


  def showMonitor(plotFreq : Int = 100) = {
	  val window = new JFrame
	  
	  val tabbedPane = new JTabbedPane();
	  
	  val dot = showDot()
	  //panel.setBackground(new Color(255,255,255))
	  val errorPlot = new ActivePlot()
	  errorPlot.setWindowedAverage(plotFreq)
	  for(n <- internalNodes) errorPlot.newDataset(n._1)
	  tabbedPane.addTab("TD Errors",errorPlot.getContentPane())	  
	  tabbedPane.addTab("Network",dot)	  
      //======================================================================
	  //create plots to display weights
	  var weightPlots : List[(ActivePlot,MultivariateTD)] = Nil
	  for(node <- internalNodes){
            node._2.answerPredictor match{
            case td : MultivariateTD => {
            	val plot = new ActivePlot()
            	plot.setWindowedAverage(plotFreq)
            	  if(td.bias)
            		plot.newDataset("bias")
            	  for(i <- node._2.inputNodes)
            		  plot.newDataset(i.name)
            	  weightPlots ::= (plot,td)
            	  
    
            	  tabbedPane.addTab(node._1+" Weights", plot.getContentPane());		  
              }
              case _ => ()
            }
	  }
	  
	  //======================================================================
	  
	  var obsPlots : List[(ActivePlot,InternalNode)] = Nil
	  for(node <- internalNodes){
		  val targets = node._2.targetNodes
		  if(targets.length == 1 && targets(0)==LeafNodes.ObservationNode){
			  val plot = new ActivePlot()
			  plot.setLinesEnabled(false);
			  plot.setMarkersEnabled(true);
			  for(i <- 0 until observationDimension){
				  plot.newDataset("Predict "+i)
				  plot.newDataset("True "+i)
			  }
			  
			  obsPlots ::= (plot,node._2)
			  tabbedPane.addTab(node._1+" Predictions", plot.getContentPane());
		  }
	  }
	  
	  //======================================================================
	  
	  tabbedPane.setVisible(true)
	  window.add(tabbedPane)
	  window.setSize(800,600)
	  window.setVisible(true)
	  
	  var steps = 0

	  updateMonitor = () => {
            //------------------------------
            //update errors
	    val nodes = internalNodes.toList
	    for(i <- 0 until nodes.size){
	      errorPlot.addPointToSeries(steps,lastErrors(nodes(i)._1),i)
	    }
            //------------------------------
            //show weights (for td)
            for(p <- weightPlots) {
              for(i <- 0 until p._2.theta.numCols)
                p._1.addPointToSeries(steps, p._2.theta(0, i), i)  
            }
            
            //------------------------------
            //show observation predictions (for td)
            if(steps % plotFreq == 0)
            for(p <- obsPlots) {
            	val node = p._2
              for(i <- 0 until observationDimension){
                p._1.addPointToSeries(steps, node.getY(ot)(i), i*2)  
                p._1.addPointToSeries(steps, otp1.features(i), i*2+1)
              }
            }

            //------------------------------
	    steps = steps+1
	  }	  
  }

  //============================================================
}

object TDNet{
  
  def getInternalNodes(prototypes : List[InternalNodePrototype], observationDimension : Int, numActions : Int) :
	  SortedMap[String, InternalNode] = {
		  val nodes = getNodes(prototypes,observationDimension,numActions)
		  var l : List[(String,InternalNode)] = Nil
		  for((k : String, n : Node) <- nodes){
			  n match{
			  case i : InternalNode => l ::=((k, i))
			  case _ => ()
			  }
		  }		  
		  (new TreeMap[String,InternalNode]) ++ l
  }
  
  def getNodes(prototypes : List[InternalNodePrototype], observationDimension : Int, numActions : Int) : 
	  SortedMap[String, Node] = {
		  val directory : Map[NodePrototype,Node] = new HashMap[NodePrototype, Node]		  
		  var nodes = (for(p <- prototypes)
			  yield p.generate(observationDimension,numActions,prototypes,Some(directory)).flatten()).flatten
		  var thin : SortedMap[String,Node]= new TreeMap[String,Node]
		  thin ++= (for(n <- nodes) yield (n.name,n))		
		  thin
  }
}



class TDNetLearner(root: InternalNodePrototype)
  extends CanLearn with Imprintable {
  var net : TDNet = null
  
  var truth: List[Feats] = Nil

  def onImprint() {
    val oDim = imprintedO.features.length
    this.net = new TDNet(root,oDim)
  }

  override def learn(otm1 : Observation, atm1 : Action, ot : Observation) = {
    this.net.learn(otm1,atm1)      
  }
}
