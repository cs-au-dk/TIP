package tip.ast

import scala.collection.mutable
import scala.language.implicitConversions

object AstOps {

  implicit class AstOp(n: AstNode) {

    def containsInvocation: Boolean = {
      var found = false
      val invocationFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case id: ACallFuncExpr =>
              found = true
            case _ => visitChildren(node, null)
          }
        }
      }
      invocationFinder.visit(n, null)
      found
    }

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

    /**
     * Returns the set of identifiers appearing in the node
     */
    def appearingMallocs: Set[AMalloc] = {
      val mallocs = mutable.Set[AMalloc]()
      val mallocsFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case malloc: AMalloc => mallocs += malloc
            case _ => visitChildren(node, null)
          }
        }
      }
      mallocsFinder.visit(n, null)
      mallocs.toSet
    }

    /**
     * Returns the set of constants appearing in the node
     */
    def constants: Set[ANumber] = {
      val numbers = mutable.Set[ANumber]()
      val numFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case num: ANumber => numbers += num
            case _ => visitChildren(node, null)
          }
        }
      }
      numFinder.visit(n, null)
      numbers.toSet
    }

    /**
     * Returns the set of expressions appearing in the node
     */
    def appearingExpressions: Set[AExpr] = {
      val exps = mutable.Set[AExpr]()
      val expFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case exp: ABinaryOp =>
              exps += exp
              visitChildren(exp, null)
            case _ => visitChildren(node, null)
          }
        }
      }
      expFinder.visit(n, null)
      exps.toSet
    }

  }

  /**
   * An implicit class lifting the [[AstNode]] n into an unlabelled one.
   * An unlabelled node represents n without the offset, so coalescing terms that look
   * the same but appear in different part of the code.
   *
   * For example UnlabelledNode(3 + x) == UnlabelledNode(3 + x) even though the first
   * 3 + x appears at offset 5 and the second at offset 30.
   */
  implicit class UnlabelledNode[X <: AstNode](val n: X) {

    /**
     * The members of the node, excluding the location
     */
    def members = n.asInstanceOf[Product].productIterator.filter { x => if (x.isInstanceOf[Loc]) false else true }.toList

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case n: UnlabelledNode[_] =>
          (this.n, n.n) match {
            case (idt: AIdentifier, ido: AIdentifier) =>
              idt.meta.definition.get == ido.meta.definition.get
            case _ =>
              if (this.getClass != n.getClass)
                false
              else {
                this.members.zip(n.members).foldLeft(true) { (a, p) =>
                  (p._1, p._2) match {
                    case (p1: AstNode, p2: AstNode) =>
                      a && (p1: UnlabelledNode[_]) == (p2: UnlabelledNode[_])
                    case (_, _) => a && p._1 == p._2
                  }
                }
              }
          }
        case _ => false
      }
    }

    override def hashCode(): Int = n.getClass.hashCode * (members map {
      case id: AIdentifier => id.meta.definition.get.hashCode()
      case x => x.hashCode()
    }).product

    override def toString: String = s"$n"
  }

}

