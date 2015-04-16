package tip.graph

import tip.ast._
import tip.ast.AstOps._

import scala.collection.mutable

/**
 * Operations that can be performed on a node [[GNode]] 
 * of a control flow graph.
 */
trait CfgAstOps {

  val n : GNode[AstNode]

  /**
   * Returns the set of identifiers declared by the node
   */
  def declaredIds: Set[AIdentifier] = {
    n match {
      case r: GRealNode[AstNode] =>
        r.data.declaredLocals
      case aux: AuxNode[AstNode] => Set()
    }    
  }

  /**
   * Returns the set of identifiers that appear in the node
   */
  def appearingIds: Set[AIdentifier] = {
    n match {
      case r: GRealNode[AstNode] =>
        r.data.appearingIds
      case aux: AuxNode[AstNode] => Set()
    }
  }
    
}

object NodeOps {
  
  /** 
   *  Implicit lifting of a [[GNode]] to a [[CfgAstOps]]
   */
  implicit def toNodeOps(node: GNode[AstNode]) = {
    new CfgAstOps {
      override val n: GNode[AstNode] = node
    }
  }
}
