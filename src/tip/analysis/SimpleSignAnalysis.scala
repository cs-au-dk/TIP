package tip.analysis

import tip.cfg.CfgOps._
import tip.cfg.{CfgNode, CfgStmtNode, ProgramCfg}
import tip.lattices.{MapLattice, SignLattice}
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.ast._
import tip.solvers.FixpointSolvers

import scala.collection.immutable.Set

/**
  * Simple intra-procedural sign analysis.
  *
  * This is a specialized version of `SignAnalysis.Intraprocedural.SimpleSolver`
  * where most of the involved traits, classes, methods, and fields have been inlined.
  */
class SimpleSignAnalysis(cfg: ProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  /**
    * The lattice of abstract values.
    */
  val valuelattice = SignLattice

  /**
    * Set of declared variables, used by `statelattice`.
    */
  val declaredVars: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVarsAndParams)

  /**
    * The lattice of abstract states.
    */
  val statelattice: MapLattice[ADeclaration, SignLattice.type] = new MapLattice(declaredVars, valuelattice)

  /**
    * The program lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(domain, statelattice)

  /**
    * Abstract evaluation of expressions.
    */
  def eval(exp: AExpr, env: statelattice.Element)(implicit declData: DeclarationData): valuelattice.Element = {
    import valuelattice._
    exp match {
      case id: AIdentifier => env(id.declaration)
      case n: ANumber => num(n.value)
      case bin: ABinaryOp =>
        val left = eval(bin.left, env)
        val right = eval(bin.right, env)
        bin.operator match {
          case Eqq => eqq(left, right)
          case GreatThan => gt(left, right)
          case Divide => div(left, right)
          case Minus => minus(left, right)
          case Plus => plus(left, right)
          case Times => times(left, right)
          case _ => ???
        }
      case _: AInput => valuelattice.top
      case AUnaryOp(RefOp, _, _) | AUnaryOp(DerefOp, _, _) | ANull(_) | AAlloc(_, _) | AAccess(_, _, _) =>
        NoPointers.LanguageRestrictionViolation(s"No pointers allowed in eval $exp")
      case _: ACallFuncExpr => NoCalls.LanguageRestrictionViolation(s"No calls allowed in eval $exp")
      case _ => ???
    }
  }

  /**
    * Incoming dependencies. Used when computing the join from predecessors.
    * @param n an element from the worklist
    * @return the elements that the given element depends on
    */
  def indep(n: CfgNode): Set[CfgNode] = n.pred.toSet

  /**
    * Transfer functions for the different kinds of statements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    NoPointers.assertContainsNode(n.data)
    NoCalls.assertContainsNode(n.data)
    NoRecords.assertContainsNode(n.data)
    n match {
      case r: CfgStmtNode =>
        r.data match {
          // var declarations
          case varr: AVarStmt => ??? //<--- Complete here

          // assignments
          case AAssignStmt(id: AIdentifier, right, _) => ??? //<--- Complete here
          case AAssignStmt(_: AUnaryOp, _, _) => NoPointers.LanguageRestrictionViolation(s"${r.data} not allowed")

          // all others: like no-ops
          case _ => s
        }
      case _ => s
    }
  }

  /**
    * The constraint function for individual elements in the map domain.
    * First computes the join of the incoming elements and then applies the transfer function.
    * @param n the current location in the map domain
    * @param x the current lattice element for all locations
    * @return the output sublattice element
    */
  def funsub(n: CfgNode, x: lattice.Element): lattice.sublattice.Element =
    localTransfer(n, join(n, x))

  /**
    * Computes the least upper bound of the incoming elements.
    */
  def join(n: CfgNode, o: lattice.Element): lattice.sublattice.Element = {
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))
  }

  /**
    * The function for which the least fixpoint is to be computed.
    * Applies the sublattice constraint function pointwise to each entry.
    * @param x the input lattice element
    * @return the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element = {
    FixpointSolvers.log.verb(s"In state $x")
    domain.foldLeft(lattice.bottom)(
      (m, a) =>
        m + (a -> {
          FixpointSolvers.log.verb(s"Processing $a")
          funsub(a, x)
        })
    )
  }

  /**
    * The basic Kleene fixpoint solver.
    */
  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var t = x
    do {
      t = x
      x = fun(x)
    } while (x != t)
    x
  }
}
