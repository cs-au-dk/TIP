package tip.parser

import org.parboiled2._
import scala.language.implicitConversions
import tip.ast._
import scala.collection._

trait Comments { this: Parser =>

  var lastBreaks = mutable.MutableList[Int](0)

  implicit def offset2Loc(i: Int): Loc = {
    val idx = lastBreaks.lastIndexWhere(brk => brk <= i)
    Loc(idx + 1, i - lastBreaks(idx) + 1)
  }

  def NewLine: Rule0 = rule {
    (str("\r\n") | str("\n\r") | str("\r") | str("\n")) ~> { () =>
      lastBreaks += cursor; ()
    }
  }

  def NonClosing: Rule0 = rule {
    zeroOrMore("*" ~ !"/" | noneOf("*\n\r") | NewLine)
  }

  def BlockComment: Rule0 = rule("/*" ~ (BlockComment | NonClosing) ~ "*/")

  def Comment: Rule0 = rule(BlockComment | "//" ~ zeroOrMore(noneOf("\n\r")) ~ NewLine)
}

/**
  *  Parser for TIP programs, implemented using the parboiled2 parser generator ([[http://parboiled2.org]]).
  */
class TipParser(val input: ParserInput) extends Parser with Comments {
  def InputLine = rule {
    Program ~ EOI
  }

  def WS = rule {
    NewLine | CharPredicate(" \t\f") | Comment
  }

  def keywords = rule {
    "alloc" | "input" | "while" | "if" | "else" | "var" | "return" | "null" | "output"
  }

  def OptSpace = rule {
    zeroOrMore(WS)
  }

  implicit def wspStr(s: String): Rule0 = rule {
    OptSpace ~ str(s) ~ OptSpace
  }

  def AssignableExpression: Rule1[AstNode.Assignable] = rule {
    (Identifier | LeftHandUnaryPointerExpression) ~> { x: AstNode =>
      x match {
        case id: AIdentifier => Left(id)
        case AUnaryOp(DerefOp, t, l) => Right(AUnaryOp(DerefOp, t, l))
        case _ => ??? // unexpected, this should never be the case
      }
    }
  }

  def Expression: Rule1[AExpr] = rule {
    Operation ~
      optional(
        push(cursor) ~ ">" ~ Operation ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(GreatThan, e1, e2, cur))
          | push(cursor) ~ "==" ~ Operation ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Eqq, e1, e2, cur))
      )
  }

  def Operation: Rule1[AExpr] = rule {
    Term ~
      optional(
        push(cursor) ~ "+" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Plus, e1, e2, cur))
          | push(cursor) ~ "-" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Minus, e1, e2, cur))
      )
  }

  def Term: Rule1[AExpr] = rule {
    Atom ~ optional(
      push(cursor) ~ "*" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Times, e1, e2, cur))
        | push(cursor) ~ "/" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Divide, e1, e2, cur))
    )
  }

  def Atom: Rule1[AExpr] = rule {
    (FunApp
      | Number
      | Parens
      | Identifier
      | push(cursor) ~ wspStr("input") ~> ((cur: Int) => AInput(cur))
      | PointersExpression)
  }

  def Parens = rule {
    "(" ~ Expression ~ ")" ~> (a => a)
  }

  def Number: Rule1[AExpr] = rule {
    push(cursor) ~ capture(Digits) ~> ((cur: Int, n: String) => ANumber(n.toInt, cur))
  }

  def Digits = rule {
    optional("-") ~ oneOrMore(CharPredicate.Digit)
  }

  def Identifier: Rule1[AIdentifier] = rule {
    push(cursor) ~ OptSpace ~ !keywords ~ capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> ((cur: Int, id: String) =>
                                                                                                                  AIdentifier(id, cur))
  }

  def IdentifierDeclaration: Rule1[AIdentifierDeclaration] = rule {
    push(cursor) ~ OptSpace ~ !keywords ~ capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> ((cur: Int, id: String) =>
                                                                                                                  AIdentifierDeclaration(id, cur))
  }

  def PointersExpression = rule {
    ZeraryPointerExpression | UnaryPointerExpression
  }

  def ZeraryPointerExpression = rule {
    (push(cursor) ~ wspStr("alloc") ~> ((cur: Int) => AAlloc(cur))
      | push(cursor) ~ wspStr("null") ~> ((cur: Int) => ANull(cur)))
  }

  def UnaryPointerExpression = rule {
    ((push(cursor) ~ "&" ~ Identifier ~> ((cur: Int, id: AIdentifier) => AUnaryOp(RefOp, id, cur)))
      | (push(cursor) ~ "*" ~ Atom ~> ((cur: Int, e: AExpr) => AUnaryOp(DerefOp, e, cur))))
  }

  def LeftHandUnaryPointerExpression: Rule1[AUnaryOp[DerefOp.type]] = rule {
    push(cursor) ~ "*" ~ Atom ~> ((cur: Int, e: AExpr) => AUnaryOp[DerefOp.type](DerefOp, e, cur))
  }

  def Program: Rule1[AProgram] = rule {
    push(cursor) ~ zeroOrMore(TipFunction) ~> ((cur: Int, funList: Seq[AFunDeclaration]) => AProgram(funList.toList, cur))
  }

  def TipFunction: Rule1[AFunDeclaration] = rule {
    push(cursor) ~ Identifier ~ "(" ~ zeroOrMore(IdentifierDeclaration).separatedBy(",") ~ ")" ~ FunBlock ~> {
      (cur: Int, id: AIdentifier, args: Seq[AIdentifierDeclaration], b: AFunBlockStmt) =>
        AFunDeclaration(id.value, args.toList, b, cur)
    }
  }

  def Statements: Rule1[Seq[AStmtInNestedBlock]] = rule {
    zeroOrMore(Statement)
  }

  def VarStatements: Rule1[Seq[AVarStmt]] = rule {
    zeroOrMore(Declaration)
  }

  def Statement: Rule1[AStmtInNestedBlock] = rule {
    Output | Assigment | Block | While | If | Error
  }

  def Assigment: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ AssignableExpression ~ "=" ~ Expression ~ ";" ~> { (cur: Int, e1: AstNode.Assignable, e2: AExpr) =>
      AAssignStmt(e1, e2, cur)
    }
  }

  def Block: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ "{" ~ Statements ~ "}" ~> ((cur: Int, body: Seq[AStmtInNestedBlock]) => ANestedBlockStmt(body.toList, cur))
  }

  def FunBlock: Rule1[AFunBlockStmt] = rule {
    push(cursor) ~ "{" ~ VarStatements ~ Statements ~ Return ~ "}" ~> ((cur: Int, declarations: Seq[AVarStmt], others: Seq[AStmtInNestedBlock],
                                                                        ret: AReturnStmt) =>
                                                                         AFunBlockStmt(declarations.toList, others.toList, ret, cur))
  }

  def Declaration: Rule1[AVarStmt] = rule {
    push(cursor) ~ "var" ~ oneOrMore(IdentifierDeclaration)
      .separatedBy(",") ~ ";" ~> ((cur: Int, idSeq: Seq[AIdentifierDeclaration]) => AVarStmt(idSeq.toList, cur))
  }

  def While: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ "while" ~ "(" ~ Expression ~ ")" ~ Statement ~> ((cur: Int, e: AExpr, b: AStmtInNestedBlock) => AWhileStmt(e, b, cur))
  }

  def If: Rule1[AStmtInNestedBlock] = rule {
    (push(cursor) ~ "if" ~ "(" ~ Expression ~ ")" ~ Statement ~ optional("else" ~ Statement)) ~> {
      (cur: Int, e: AExpr, bt: AStmtInNestedBlock, bf: Option[AStmtInNestedBlock]) =>
        {
          AIfStmt(e, bt, bf, cur)
        }
    }
  }

  def Output: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ "output" ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AOutputStmt(e, cur))
  }

  def Return: Rule1[AReturnStmt] = rule {
    push(cursor) ~ "return" ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AReturnStmt(e, cur))
  }

  def Error: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ "error" ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AErrorStmt(e, cur))
  }

  def FunApp: Rule1[AExpr] = rule {
    (push(cursor) ~ Parens ~ FunActualArgs ~> ((cur: Int, fun: AExpr, args: Seq[AExpr]) => ACallFuncExpr(fun, args.toList, cur))
      | push(cursor) ~ Identifier ~ FunActualArgs ~> ((cur: Int, id: AIdentifier, args: Seq[AExpr]) => ACallFuncExpr(id, args.toList, cur)))
  }

  def FunActualArgs: Rule1[Seq[AExpr]] = rule {
    "(" ~ zeroOrMore(Expression).separatedBy(",") ~ ")"
  }
}
