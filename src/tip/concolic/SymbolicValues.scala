package tip.concolic

import tip.ast._
import tip.interpreter.ValueSpecification

object SymbolicValues extends ValueSpecification {

  val noLoc = Loc(-1, -1)

  var lastLoc = 0

  case class SymbIntValue(i: Int, symbolic: AExpr) extends IntValue

  /**
    * Null: expressible and storable, but not denotable
    */
  case class ConcreteNullValue() extends NullValue

  /**
    * Reference: expressible, storable and denotable
    */
  case class ConcreteReferenceValue(i: Int) extends ReferenceValue

  /**
    * A procedure: denotable and storable
    */
  case class ConcreteFunValue(fun: AFunDeclaration) extends FunValue

  val nullValue = ConcreteNullValue()

  val returnLoc = ConcreteReferenceValue(0)

  /**
    * Creates a new reference
    */
  def newLoc(): ReferenceValue = { lastLoc += 1; ConcreteReferenceValue(lastLoc) }

  def constInt(i: Int): IntValue = SymbIntValue(i, ANumber(i, noLoc))

  def eqq(x: EValue, y: EValue): IntValue = (x, y) match {
    case (i1: SymbIntValue, i2: SymbIntValue) =>
      SymbIntValue(if (i1.i == i2.i) 1 else 0, ABinaryOp(Eqq, i1.symbolic, i2.symbolic, noLoc))
    case (x: EValue, y: EValue) =>
      // Equality of non-number is not supported by the solver, use concrete symbolic value.
      // Concolic testing becomes incomplete
      val num = if (x == y) 1 else 0
      SymbIntValue(num, ANumber(num, noLoc))
  }
  def eqqInt(x: IntValue, y: IntValue): Boolean = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      x.i == y.i
  }
  def divideInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      // Division not supported by the solver, use concrete symbolic value
      // Concolic testing becomes incomplete
      SymbIntValue(x.i / y.i, ANumber(x.i / y.i, noLoc))
  }
  def greatThanInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      SymbIntValue(if (x.i > y.i) 1 else 0, ABinaryOp(GreatThan, x.symbolic, y.symbolic, noLoc))
  }
  def timesInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      SymbIntValue(x.i * y.i, ABinaryOp(Times, x.symbolic, y.symbolic, noLoc))
  }
  def plusInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      SymbIntValue(x.i + y.i, ABinaryOp(Plus, x.symbolic, y.symbolic, noLoc))
  }
  def minusInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: SymbIntValue, y: SymbIntValue) =>
      SymbIntValue(x.i - y.i, ABinaryOp(Minus, x.symbolic, y.symbolic, noLoc))
  }

  def mkFun(f: AFunDeclaration): FunValue = ConcreteFunValue(f)

}
