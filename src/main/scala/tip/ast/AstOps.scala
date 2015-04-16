package tip.ast

import scala.collection.mutable


trait AstOps {

  val n: AstNode

  /**
   * Return the set of identifiers declared by the node
   */
  def declaredLocals: Set[AIdentifier] = {
    n match {
      case varr: AVarStmt => varr.declIds.toSet
      case _ => Set()
    }
  }

  /**
   * Returns the set of identifiers appearing in the node
   */
  def appearingIds: Set[AIdentifier] = {
    val ids = mutable.Set[AIdentifier]()
    val idFinder = new DepthFirstAstVisitor[Null] {
      override def visit(node: AstNode, arg: Null): Unit = {

        node match {
          case id: AIdentifier => id.meta.definition.get match {
            case local: AIdentifier => ids += local
            case fun: AFunDeclaration => ids += fun.name
          }
          case _ => visitChildren(node, null)
        }
      }
    }
    idFinder.visit(n, null)
    ids.toSet
  }
}


object AstOps {
  /**
   * List an [[AstNode]] to an [[AstOps]] 
   */
  implicit def toAstOps(node: AstNode): AstOps = {
    new AstOps {
      override val n: AstNode = node
    }
  }
}