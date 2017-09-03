package tip.ast

import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}

import scala.collection.mutable

/**
  * Convenience operations related to ASTs.
  */
object AstOps {

  /**
    * Special 'result' variable, for function return values.
    */
  val returnId = AIdentifierDeclaration(s"#result", Loc(0, 0))

  /**
    * An implicit class with convenience methods for collecting information in AST subtrees.
    *
    * (For information about Scala's implicit classes, see [[tip.ast.AstNodeData.AstNodeWithDeclaration]].)
    */
  implicit class AstOp(n: AstNode) {

    /**
      * Checks whether the subtree of the node contains function calls.
      */
    def containsInvocation: Boolean = {
      var found = false
      val invocationFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case _: ACallFuncExpr =>
              found = true
            case _ => visitChildren(node, null)
          }
        }
      }
      invocationFinder.visit(n, null)
      found
    }

    /**
      * Returns the set of local variable identifiers declared by the node (excluding function parameters and function identifiers).
      */
    def declaredLocals: Set[ADeclaration] = {
      n match {
        case varr: AVarStmt => varr.declIds.toSet
        case _ => Set()
      }
    }

    /**
      * Returns the set of identifier declarations appearing in the subtree of the node.
      */
    def appearingIds(implicit declData: DeclarationData): Set[ADeclaration] = {
      val ids = mutable.Set[ADeclaration]()
      val idFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case id: AIdentifier =>
              id.declaration match {
                case local: AIdentifierDeclaration => ids += local
                case fun: AFunDeclaration => ids += fun
              }
            case decl: AIdentifierDeclaration => ids += decl
            case fun: AFunDeclaration => ids += fun
            case _ =>
          }
          visitChildren(node, null)
        }
      }
      idFinder.visit(n, null)
      ids.toSet
    }

    /**
      * Returns the set of allocs appearing in the subtree of the node.
      */
    def appearingAllocs: Set[AAlloc] = {
      val allocs = mutable.Set[AAlloc]()
      val allocsFinder = new DepthFirstAstVisitor[Null] {
        override def visit(node: AstNode, arg: Null): Unit = {
          node match {
            case alloc: AAlloc => allocs += alloc
            case _ => visitChildren(node, null)
          }
        }
      }
      allocsFinder.visit(n, null)
      allocs.toSet
    }

    /**
      * Returns the set of constants appearing in the subtree of the node.
      */
    def appearingConstants: Set[ANumber] = {
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
      * Returns the set of expressions appearing in the subtree of the node.
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
    * An implicit class for lifting the given [[AstNode]] `n` into an unlabelled one.
    * The unlabelled node represents `n` but without the source code location.
    * Thereby two nodes become "equal" if they are structurally the same even though they appear in different parts of the code.
    * Identifiers are compared using their declarations.
    *
    * (For information about Scala's implicit classes, see [[tip.ast.AstNodeData.AstNodeWithDeclaration]].)
    */
  implicit class UnlabelledNode[X <: AstNode](val n: X)(implicit declData: DeclarationData) {

    /**
      * The members of the node, excluding `loc`.
      */
    lazy val nonLocMembers =
      n.productIterator.filter { x =>
        if (x.isInstanceOf[Loc]) false else true
      }.toList

    /**
      * Compares objects structurally, but ignores `loc`.
      * Identifiers are compared using their declarations.
      */
    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case n: UnlabelledNode[_] =>
          (this.n, n.n) match {
            case (idt: AIdentifier, ido: AIdentifier) =>
              idt.declaration == ido.declaration
            case _ =>
              if (this.getClass != n.getClass)
                false
              else {
                this.nonLocMembers.zip(n.nonLocMembers).foldLeft(true) { (a, p) =>
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

    override lazy val hashCode: Int =
      n.getClass.hashCode * (nonLocMembers map {
        case id: AIdentifier => id.declaration.hashCode()
        case x => x.hashCode()
      }).product

    override def toString: String = n.toString
  }
}
