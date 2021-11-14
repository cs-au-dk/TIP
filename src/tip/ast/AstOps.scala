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
      val invocationFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case _: ACallFuncExpr =>
              found = true
            case _ => visitChildren(node, ())
          }
      }
      invocationFinder.visit(n, ())
      found
    }

    /**
      * Returns the set of local variable identifiers declared by the node (excluding function parameters and function identifiers).
      */
    def declaredLocals: Set[ADeclaration] =
      n match {
        case varr: AVarStmt => varr.declIds.toSet
        case _ => Set()
      }

    /**
      * Returns the set of identifiers (represented by their declarations) appearing in the subtree of the node.
      */
    def appearingIds(implicit declData: DeclarationData): Set[ADeclaration] = {
      val ids = mutable.Set[ADeclaration]()
      val idFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit = {
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
          visitChildren(node, ())
        }
      }
      idFinder.visit(n, ())
      ids.toSet
    }

    /**
      * Returns the set of allocs appearing in the subtree of the node.
      */
    def appearingAllocs: Set[AAlloc] = {
      val allocs = mutable.Set[AAlloc]()
      val allocsFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case alloc: AAlloc => allocs += alloc
            case _ => visitChildren(node, ())
          }
      }
      allocsFinder.visit(n, ())
      allocs.toSet
    }

    /**
      * Returns the set of constants appearing in the subtree of the node.
      */
    def appearingConstants: Set[ANumber] = {
      val numbers = mutable.Set[ANumber]()
      val numFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case num: ANumber => numbers += num
            case _ => visitChildren(node, ())
          }
      }
      numFinder.visit(n, ())
      numbers.toSet
    }

    /**
      * Returns the set of expressions appearing in the subtree of the node.
      */
    def appearingExpressions: Set[AExpr] = {
      val exps = mutable.Set[AExpr]()
      val expFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case exp: ABinaryOp =>
              exps += exp
              visitChildren(exp, ())
            case _ => visitChildren(node, ())
          }
      }
      expFinder.visit(n, ())
      exps.toSet
    }

    /**
      * Returns the set of non-'input' expressions appearing in the subtree of the node.
      */
    def appearingNonInputExpressions: Set[AExpr] =
      appearingExpressions.filterNot(e => e.containsInput)

    /**
      * Checks whether the subtree of the node contains an 'input' expression.
      */
    def containsInput: Boolean = {
      var res = false
      new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case _: AInput => res = true;
            case _ => visitChildren(node, ())
          }
      }.visit(n, ())
      res
    }

    /**
      * Returns the set of record field names appearing in the subtree of the node.
      */
    def appearingFields: Set[String] = {
      val fields = mutable.Set[String]()
      val expFinder = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit =
          node match {
            case exp: AFieldAccess =>
              fields += exp.field
              visitChildren(exp, ())
            case rec: ARecord =>
              fields ++= rec.fields.map { _.field }
              visitChildren(rec, ())
            case as: AAssignStmt =>
              as.left match {
                case dfw: ADirectFieldWrite =>
                  fields += dfw.field
                case ifw: AIndirectFieldWrite =>
                  fields += ifw.field
                case _ =>
              }
              visitChildren(as, ())
            case _ => visitChildren(node, ())
          }
      }
      expFinder.visit(n, ())
      fields.toSet
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
    lazy val nonLocMembers: List[Any] =
      n.productIterator.filter { x =>
        if (x.isInstanceOf[Loc]) false else true
      }.toList

    /**
      * Compares objects structurally, but ignores `loc`.
      * Identifiers are compared using their declarations.
      */
    override def equals(obj: scala.Any): Boolean =
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

    override lazy val hashCode: Int =
      n.getClass.hashCode * (nonLocMembers map {
        case id: AIdentifier => id.declaration.hashCode()
        case x => x.hashCode()
      }).product

    override def toString: String = n.toString
  }
}
