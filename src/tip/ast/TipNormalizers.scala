package tip.ast

class Normalizer {

  var lastUid: Int = 0
  def newVariable(): String = {
    lastUid += 1
    s"tmp_$lastUid"
  }

  /** The list of declarations that have to be added in the current function. */
  val declarations: scala.collection.mutable.ListBuffer[AIdentifierDeclaration] = scala.collection.mutable.ListBuffer.empty

  /**
    * Adds a declaration.
    */
  def addDeclaration(decl: AIdentifierDeclaration): Unit = declarations += decl

  /**
    * The list of statements to be added in the current block.
    * This will mostly be assignments but can be any statement that can be in a nested block.
    */
  val statements: scala.collection.mutable.ListBuffer[AStmtInNestedBlock] = scala.collection.mutable.ListBuffer.empty

  /**
    * Adds a statement.
    */
  def addStatement(stmt: AStmtInNestedBlock): Unit = statements += stmt

  /**
    * Normalizes an AExpr.
    */
  def normalizeExpr(e: AExpr): AExpr =
    e match {
      case _: AIdentifier => e
      case uop: AUnaryOp => uop.copy(subexp = normalizeExpr(uop.subexp))
      case bop: ABinaryOp => bop.copy(left = normalizeExpr(bop.left), right = normalizeExpr(bop.right))
      case r: ARecord => r.copy(fields = r.fields.map(normalizeRecordField))
      case call: ACallFuncExpr => call.copy(targetFun = normalizeExpr(call.targetFun), args = call.args.map(normalizeExpr))
      case _ => e
    }

  /**
    * Normalizes an AExpr into an AIdentifier. This is not used by the Normalizer class but useful for subclasses, as this is a common operation.
    */
  def normalizeToIdentifier(right: AExpr): AIdentifier =
    right match {
      case id: AIdentifier => id
      case _ =>
        val tmpVar = newVariable()
        val id = AIdentifier(tmpVar, right.loc)
        addDeclaration(AIdentifierDeclaration(tmpVar, right.loc))
        addStatement(normalizeStmtInNestedBlock(AAssignStmt(AIdentifier(tmpVar, right.loc), right, right.loc)))
        id
    }

  /**
    * Normalizes an ARecordField.
    */
  def normalizeRecordField(f: ARecordField): ARecordField =
    f.copy(exp = normalizeExpr(f.exp))

  /**
    * Normalizes an Assignable.
    */
  def normalizeAssignable(e: Assignable): Assignable =
    e match {
      case _: AIdentifier | _: ADirectFieldWrite => e
      case dw: ADerefWrite => dw.copy(exp = normalizeExpr(dw.exp))
      case ifw: AIndirectFieldWrite => ifw.copy(exp = normalizeExpr(ifw.exp))
    }

  /**
    * Helper function to insert statements if there are any to insert before `stmt`. Otherwise, returns the same statement.
    */
  def nestedBlock(stmt: AStmtInNestedBlock): AStmtInNestedBlock =
    if (statements.isEmpty) { stmt } else {
      val res = ANestedBlockStmt(statements.toList :+ stmt, stmt.loc)
      statements.clear()
      res
    }

  /**
    * Normalizes an AStmtInNestedBlock.
    */
  def normalizeStmtInNestedBlock(stmt: AStmtInNestedBlock): AStmtInNestedBlock =
    stmt match {
      case stmt: AAssignStmt =>
        nestedBlock(stmt.copy(left = normalizeAssignable(stmt.left), right = normalizeExpr(stmt.right)))
      case stmt: ANestedBlockStmt =>
        stmt.copy(body = stmt.body.map(normalizeStmtInNestedBlock))
      case stmt: AIfStmt =>
        // It is important to first normalizes the if/else branches before calling nestedBlock, so that added statements for each branch remain in the corresponding branch, and added statements for the guard are added before the if.
        val ifBranch2 = normalizeStmtInNestedBlock(stmt.ifBranch)
        val elseBranch2 = stmt.elseBranch.map(normalizeStmtInNestedBlock)
        nestedBlock(stmt.copy(guard = normalizeExpr(stmt.guard), ifBranch = ifBranch2, elseBranch = elseBranch2))
      case stmt: AOutputStmt =>
        nestedBlock(stmt.copy(exp = normalizeExpr(stmt.exp)))
      case stmt: AErrorStmt =>
        nestedBlock(stmt.copy(exp = normalizeExpr(stmt.exp)))
      case stmt: AWhileStmt =>
        val innerBlock2 = normalizeStmtInNestedBlock(stmt.innerBlock)
        nestedBlock(stmt.copy(guard = normalizeExpr(stmt.guard), innerBlock = innerBlock2))
    }

  /**
    * Normalizes an AReturnStmt.
    */
  def normalizeReturnStmt(ret: AReturnStmt): AReturnStmt =
    ret.copy(exp = normalizeExpr(ret.exp))

  /**
    * Normalizes an AFunBlockStmt.
    */
  def normalizeFunBlockStmt(stmt: AFunBlockStmt): AFunBlockStmt = {
    // Normalizes its body
    val others2 = stmt.others.map(normalizeStmtInNestedBlock)
    // And normalizes its return statement
    val ret2 = normalizeReturnStmt(stmt.ret)
    // Add declarations to the function if needed
    val declarations2 = if (declarations.isEmpty) {
      stmt.declarations
    } else {
      stmt.declarations :+ AVarStmt(declarations.toList, stmt.loc)
    }
    // Add statements before the return statement if needed
    val others3 = if (statements.isEmpty) {
      others2
    } else {
      others2 :+ ANestedBlockStmt(statements.toList, stmt.loc)
    }
    declarations.clear()
    statements.clear()
    stmt.copy(declarations = declarations2, others = others3, ret = ret2)
  }

  /**
    * Normalizes an AFunDeclaration.
    */
  def normalizeDeclaration(decl: AFunDeclaration): AFunDeclaration =
    decl.copy(stmts = normalizeFunBlockStmt(decl.stmts))

  /**
    * Normalizes an AProgram.
    */
  def normalizeProgram(program: AProgram): AProgram =
    program.copy(funs = program.funs.map(normalizeDeclaration))
}

/**
  * Combines two normalizers, running `normalizer1` followed by `normalizer2` on the input program.
  */
class CombineNormalizers(normalizer1: Normalizer, normalizer2: Normalizer) extends Normalizer {
  override def normalizeProgram(program: AProgram): AProgram =
    normalizer2.normalizeProgram(normalizer1.normalizeProgram(program))
}

/**
  * A normalizer that does nothing.
  */
object NoNormalizer extends Normalizer {
  // We don't *have* to redefine normalizeProgram, because its definition in Normalizer ends up returning the same program, but this makes things clearer.
  override def normalizeProgram(program: AProgram): AProgram = program
}

/**
  * Normalizes return statements so that we only have returns of the form `return id` where id is an identifier.
  */
object ReturnsNormalizer extends Normalizer {
  override def normalizeReturnStmt(ret: AReturnStmt): AReturnStmt =
    // [[return e]] becomes [[return id]]
    ret.copy(exp = normalizeToIdentifier(ret.exp))
}

/**
  * Normalizes function calls to fit into the NormalizedCalls sub-language, in which all function calls should have the form `id = id(id1, id2, ...)`.
  */
object CallsNormalizer extends Normalizer {
  override def normalizeExpr(e: AExpr): AExpr =
    e match {
      case f: ACallFuncExpr =>
        // Normalizes the function call, but also replaces it by an identifier assigned to its result.
        // The only case where this replacement is not done is handled by normalizeStmtInNestedBlock, when we already have the form [[id = e(e1, e2, ...)]]
        normalizeToIdentifier(normalizeFunctionCall(f))
      case _ => super.normalizeExpr(e)
    }

  def normalizeFunctionCall(f: ACallFuncExpr): ACallFuncExpr =
    // [[e(e1, e2, ...)]] becomes [[id(id1, id2, ...)]]
    f.copy(targetFun = normalizeToIdentifier(f.targetFun), args = f.args.map(normalizeToIdentifier))

  override def normalizeStmtInNestedBlock(stmt: AStmtInNestedBlock): AStmtInNestedBlock =
    stmt match {
      case AAssignStmt(left: AIdentifier, right: ACallFuncExpr, loc) =>
        // [[id = e(e1, e2, ...)]] form, normalize the call e(e1, e2, ...) to id(id1, id2, ...)
        nestedBlock(AAssignStmt(left, normalizeFunctionCall(right), loc))
      case _ =>
        // Other cases are handled by normalizeExpr.
        super.normalizeStmtInNestedBlock(stmt)
    }
}

/**
  * Normalize pointers to fit in the NormalizedForPointsToAnalysis sub-language.
  * In that sub-language, the only allowed pointer statements are the following:
  * id = alloc P where P is null or an integer constant
  * id1 = &id2
  * id1 = id2
  * id1 = *id2
  * *id1 = id2
  * id = null
  */
object PointersNormalizer extends Normalizer {

  /**
    * Normalizes the left-hand side of an assignment so that it has the form id, *id, id.id, or (*id).id.
    */
  def normalizeLeft(left: Assignable): Assignable =
    left match {
      case AIdentifier(_, _) => left
      case ADerefWrite(exp, loc) =>
        exp match {
          case AIdentifier(_, _) => left
          case _ =>
            val tmpVar = newVariable()
            val id = AIdentifier(tmpVar, loc)
            addDeclaration(AIdentifierDeclaration(tmpVar, loc))
            addStatement(normalizeStmtInNestedBlock(AAssignStmt(id, exp, loc)))
            ADerefWrite(id, loc)
        }
      case ADirectFieldWrite(_, _, _) => left
      case AIndirectFieldWrite(exp, field, loc) =>
        exp match {
          case AIdentifier(_, _) => left
          case _ =>
            val tmpVar = newVariable()
            val id = AIdentifier(tmpVar, loc)
            addDeclaration(AIdentifierDeclaration(tmpVar, loc))
            addStatement(normalizeStmtInNestedBlock(AAssignStmt(id, exp, loc)))
            AIndirectFieldWrite(id, field, loc)
        }
    }

  /**
    * Normalizes the right-hand side of an assignment so that it has one of the form alloc P, null, &id, *id, or id.
    */
  def normalizeRight(right: AExpr): AExpr =
    right match {
      case op: AUnaryOp =>
        op.copy(subexp = normalizeToIdentifier(op.subexp))
      case _: AIdentifier => right
      case _: ANull => right
      case _: AAlloc => right
      case _ =>
        /* Other cases are treated as already normalized. Maybe it shouldn't be the case, but such other cases are not supported by the NormalizedForPointsToAnalysis sub-language in any case. */
        right
    }

  override def normalizeStmtInNestedBlock(stmt: AStmtInNestedBlock): AStmtInNestedBlock =
    stmt match {
      case AAssignStmt(left: AIdentifier, right, _) =>
        // [[id = right]] form, normalizes right only
        nestedBlock(AAssignStmt(left, normalizeRight(right), stmt.loc))
      case AAssignStmt(left, right, _) =>
        // [[left = right]] form where left is a unary operation, normalizes left, and normalizes right to an identifier.
        nestedBlock(AAssignStmt(normalizeLeft(left), normalizeToIdentifier(right), stmt.loc))
      case _ => super.normalizeStmtInNestedBlock(stmt)
    }
}
