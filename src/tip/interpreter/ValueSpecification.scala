package tip.interpreter

import tip.ast.AFunDeclaration

/**
  * Specification of values.
  * (Using the classic terminology by Strachey.)
  */
trait ValueSpecification {

  /**
    * Expressible values, the ones that can result from evaluation of expressions.
    *
    * In TIP, all values are expressible.
    */
  trait EValue

  /**
    * Storable values, the ones that that can be stored in mutable variables or heap cells.
    *
    * Storable values are generally a subset of expressible ones; in TIP they are the same.
    */
  type Value = EValue

  /**
    * Denotable values, the ones that represent memory locations.
    *
    * In TIP those are references to variables or heap cells.
    */
  type Location = ReferenceValue

  /**
    * Null: expressible and storable, but not denotable.
    */
  trait NullValue extends EValue

  /**
    * Integers: expressible and storable, but not denotable.
    */
  trait IntValue extends EValue {

    val i: Int
  }

  /**
    * Reference: expressible, storable and denotable.
    */
  trait ReferenceValue extends EValue

  /**
    * Functions: expressible and storable, but not denotable.
    */
  trait FunValue extends EValue {

    /**
      * The function that this value represents
      */
    val fun: AFunDeclaration
  }

  /**
    * Record values: expressible and storable, but not denotable.
    */
  trait RecordValue extends EValue {

    val fields: Map[String, EValue]
  }

  /**
    * The null value.
    */
  def nullValue: NullValue

  /**
    * The reserved location for storing return values.
    */
  def returnLoc: ReferenceValue

  /**
    * Produces a fresh location.
    */
  def newLoc(): ReferenceValue

  /**
    * Makes am integer value from a literal.
    */
  def constInt(i: Int): IntValue

  /**
    * The `==` operator on arbitrary values.
    */
  def eqq(x: EValue, y: EValue): IntValue

  /**
    * Equality of integer values.
    */
  def eqqInt(x: IntValue, y: IntValue): Boolean

  /**
    * The `/` operator on integer values.
    */
  def divideInt(x: IntValue, y: IntValue): IntValue

  /**
    * The `>=` operator on integer values.
    */
  def greatThanInt(x: IntValue, y: IntValue): IntValue

  /**
    * The `*` operator on integer values.
    */
  def timesInt(x: IntValue, y: IntValue): IntValue

  /**
    * Thr `+` operator on integer values.
    */
  def plusInt(x: IntValue, y: IntValue): IntValue

  /**
    * The `-` operator on integer values.
    */
  def minusInt(x: IntValue, y: IntValue): IntValue

  /**
    * Makes a function value from a function declaration in the program code.
    */
  def mkFun(fun: AFunDeclaration): FunValue

  /**
    * Makes a new record value.
    */
  def mkRecord(fields: Map[String, EValue]): RecordValue

}
