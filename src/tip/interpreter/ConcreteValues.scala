package tip.interpreter

import tip.ast.AFunDeclaration

/**
  * Values for concrete execution of TIP progams.
  */
object ConcreteValues extends ValueSpecification {

  var lastLoc = 0

  /**
    * Integer value.
    */
  case class ConcreteIntValue(i: Int) extends IntValue

  /**
    * Null value.
    */
  case class ConcreteNullValue() extends NullValue

  /**
    * Reference: expressible, storable and denotable
    * @param i determines the ID, i.e. the "address" being referred to
    */
  case class ConcreteReferenceValue(i: Int) extends ReferenceValue

  /**
    * Reference to field.
    */
  case class ConcreteReferenceFieldValue(i: Int, field: String) extends ReferenceValue

  /**
    * Function value.
    */
  case class ConcreteFunValue(fun: AFunDeclaration) extends FunValue

  /**
    * Record value.
    */
  case class ConcreteRecordValue(fields: Map[String, EValue]) extends RecordValue

  val nullValue = ConcreteNullValue()

  val returnLoc = ConcreteReferenceValue(0)

  def newLoc(): ReferenceValue = { lastLoc += 1; ConcreteReferenceValue(lastLoc) }

  def constInt(i: Int): IntValue = ConcreteIntValue(i)

  def eqq(x: EValue, y: EValue): IntValue = ConcreteIntValue(if (x == y) 1 else 0)

  def eqqInt(x: IntValue, y: IntValue): Boolean = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => x == y
    case _ => ???
  }

  def divideInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => ConcreteIntValue(x.i / y.i)
    case _ => ???
  }

  def greatThanInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => ConcreteIntValue(if (x.i > y.i) 1 else 0)
    case _ => ???
  }

  def timesInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => ConcreteIntValue(x.i * y.i)
    case _ => ???
  }

  def plusInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => ConcreteIntValue(x.i + y.i)
    case _ => ???
  }

  def minusInt(x: IntValue, y: IntValue): IntValue = (x, y) match {
    case (x: ConcreteIntValue, y: ConcreteIntValue) => ConcreteIntValue(x.i - y.i)
    case _ => ???
  }

  def mkFun(f: AFunDeclaration): FunValue = ConcreteFunValue(f)

  def mkRecord(fields: Map[String, EValue]) = ConcreteRecordValue(fields)
}
