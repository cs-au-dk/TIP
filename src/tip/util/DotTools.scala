package tip.util

/**
  * Generator for fresh node IDs.
  */
object IDGenerator {
  private var current: Int = 0

  def getNewId: Int = {
    current += 1
    current
  }
}

/**
  * Super-class for elements of a Graphviz dot file.
  */
abstract class DotElement {

  /**
    * Produces a dot string representation of this element.
    */
  def toDotString: String
}

/**
  * Represents a node in a Graphviz dot file.
  */
class DotNode(val id: String, val label: String, val additionalParams: Map[String, String]) extends DotElement {

  def this(label: String, additionalParams: Map[String, String] = Map()) =
    this("n" + IDGenerator.getNewId, label, additionalParams)

  def this() = this("")

  def equals(other: DotNode): Boolean = toDotString.equals(other.toDotString)

  override def toString: String = toDotString

  def toDotString: String =
    id + "[label=\"" + Output.escape(label) + "\"" +
      additionalParams.map(p => s"${p._1} = ${p._2}").mkString(",") + "]"

}

/**
  * Represents an edge between two nodes in a Graphviz dot file.
  */
class DotArrow(val fromNode: DotNode, arrow: String, val toNode: DotNode, val label: String) extends DotElement {

  def equals(other: DotArrow): Boolean = toDotString.equals(other.toDotString)

  def toDotString: String = fromNode.id + " " + arrow + " " + toNode.id + "[label=\"" + Output.escape(label) + "\"]"
}

/**
  * Represents a directed edge between two nodes in a Graphviz dot file.
  */
class DotDirArrow(fromNode: DotNode, toNode: DotNode, label: String) extends DotArrow(fromNode, "->", toNode, label) {
  def this(fromNode: DotNode, toNode: DotNode) = this(fromNode, toNode, "")
}

/**
  * Represents a Graphviz dot graph.
  */
class DotGraph(val title: String, val nodes: Iterable[DotNode], val edges: Iterable[DotArrow]) extends DotElement {

  def this(nodes: List[DotNode], edges: List[DotArrow]) = this("", nodes, edges)

  def this(title: String) = this(title, List(), List())

  def this() = this(List(), List())

  def addGraph(g: DotGraph): DotGraph = {
    val ng = g.nodes.foldLeft(this)((g, n) => g.addNode(n))
    g.edges.foldLeft(ng)((g, e) => g.addEdge(e))
  }

  def addNode(n: DotNode): DotGraph =
    if (nodes.exists(a => n.equals(a))) this
    else new DotGraph(title, nodes ++ List(n), edges)

  def addEdge(e: DotArrow): DotGraph =
    if (edges.exists(a => e.equals(a))) this
    else new DotGraph(title, nodes, edges ++ List(e))

  override def toString: String = toDotString

  def toDotString: String = "digraph " + title + "{" + (nodes ++ edges).foldLeft("")((str, elm) => str + elm.toDotString + "\n") + "}"
}
