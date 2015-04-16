package tip.parser

import org.parboiled2._
import shapeless.HNil
import tip.ast._
import scala.collection.{ mutable, immutable }

trait Comments {
  this: Parser =>

  var lastBreaks = mutable.MutableList[Int](0)

  implicit def offset2Loc(i: Int): Loc = {
    val idx = lastBreaks.lastIndexWhere(brk => brk <= i)
    Loc(idx, i - lastBreaks(idx))
  }

  def NewLine: Rule0 = rule {
    (str("\r\n") | str("\n\r") | str("\r") | str("\n")) ~> { () => lastBreaks += cursor; () }
  }

  def BlockComment: Rule0 = rule("/*" ~ (BlockComment | zeroOrMore(!"*/" ~ ANY)) ~ "*/")

  def Comment: Rule0 = rule(BlockComment | "//" ~ zeroOrMore((!NewLine ~ ANY)))
}

class TipParser(val input: ParserInput) extends Parser with Comments {
  def InputLine = rule {
    Program ~ EOI
  }

  def WS = rule {
    NewLine | CharPredicate(" \t\f") | Comment
  }

  def keywords = rule {
    "malloc" | "input" | "while" | "if" | "else" | "var" | "return" | "null" | "output"
  }

  def OptSpace = rule {
    zeroOrMore(WS)
  }

  def OptSpaceInline = rule {
    zeroOrMore(WSinline)
  }

  def WSinline = rule {
    CharPredicate(" \t\f") | Comment
  }

  implicit def wspStr(s: String): Rule0 = rule {
    OptSpace ~ str(s) ~ OptSpace
  }

  def Expression: Rule1[AExpr] = rule {
    Term ~
      optional(
        push(cursor) ~ "+" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Plus(), e1, e2, cur)())
          | push(cursor) ~ "-" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Minus(), e1, e2, cur)()))
  }

  def Term: Rule1[AExpr] = rule {
    Atom ~ optional(
      push(cursor) ~ "*" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Times(), e1, e2, cur)())
        | push(cursor) ~ "/" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Divide(), e1, e2, cur)())
        | push(cursor) ~ ">" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(GreatThan(), e1, e2, cur)())
        | push(cursor) ~ "==" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Eqq(), e1, e2, cur)()))
  }

  def Atom: Rule1[AExpr] = rule {
    (FunApp
      | Number
      | Parens
      | Identifier
      | push(cursor) ~ wspStr("input") ~> ((cur: Int) => AInput(cur)())
      | PointersExpression)
  }

  def Parens = rule {
    "(" ~ Expression ~ ")" ~> (a => a)
  }

  def Number: Rule1[AExpr] = rule {
    push(cursor) ~ capture(Digits) ~> ((cur: Int, n: String) => ANumber(n.toInt, cur)())
  }

  def Digits = rule {
    optional("-") ~ oneOrMore(CharPredicate.Digit)
  }

  def Identifier: Rule1[AIdentifier] = rule {
    push(cursor) ~ OptSpace ~ !keywords ~ capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> ((cur: Int, id: String) => AIdentifier(id, cur)())
  }

  def PointersExpression = rule {
    ZeraryPointerExpression | UnaryPointerExpression
  }

  def ZeraryPointerExpression = rule {
    (push(cursor) ~ wspStr("malloc") ~> ((cur: Int) => AMalloc(cur)())
      | push(cursor) ~ wspStr("null") ~> ((cur: Int) => ANull(cur)()))
  }

  def UnaryPointerExpression = rule {
    ((push(cursor) ~ "&" ~ Atom ~> ((cur: Int, e: AExpr) => AUnaryOp(RefOp(), e, cur)()))
      | (push(cursor) ~ "*" ~ Atom ~> ((cur: Int, e: AExpr) => AUnaryOp(DerefOp(), e, cur)())))
  }

  def Program: Rule1[AProgram] = rule {
    push(cursor) ~ zeroOrMore(
      TipFunction) ~> ((cur: Int, funList: immutable.Seq[AFunDeclaration]) => AProgram(funList, cur))
  }

  def TipFunction: Rule1[AFunDeclaration] = rule {
    push(cursor) ~ Identifier ~ "(" ~ zeroOrMore(Identifier).separatedBy(",") ~ ")" ~ Block ~> { (cur: Int, id: AIdentifier, args: immutable.Seq[AIdentifier], b: ABlockStmt) =>
      if(!b.content.last.isInstanceOf[AReturnStmt]) throw new RuntimeException(s"Missing return statement in function $id")
      AFunDeclaration(id, args, b, cur)()
    }
  }

  def Statements = rule {
    zeroOrMore(Statement)
  }

  def Statement = rule {
    Output | Assigment | Declaration | Block | While | If | Return
  }

  def Assigment: Rule1[AStmt] = rule {
    (push(cursor) ~ Expression ~ "=" ~ Expression ~ ";" ~> {(cur: Int, e1: AExpr, e2: AExpr) => 
      e1 match {
        case ase: AAssignable => AAssignStmt(ase, e2, cur) 
        case _ => throw new RuntimeException("The left side of an assignment should be assignable: i.e. a de-reference or a variable")
      }
    })
  }

  def Block: Rule1[ABlockStmt] = rule {
    push(cursor) ~ "{" ~ Statements ~ "}" ~> ((cur: Int, stmSeq: immutable.Seq[AStmt]) => ABlockStmt(stmSeq, cur))
  }

  def Declaration: Rule1[AStmt] = rule {
    push(cursor) ~ "var" ~ oneOrMore(Identifier).separatedBy(",") ~ ";" ~> ((cur: Int, idSeq: immutable.Seq[AIdentifier]) => AVarStmt(idSeq, cur))
  }

  def While: Rule1[AStmt] = rule {
    push(cursor) ~ "while" ~ "(" ~ Expression ~ ")" ~ Statement ~> ((cur: Int, e: AExpr, b: AStmt) => AWhileStmt(e, b, cur))
  }

  def If: Rule1[AStmt] = rule {
    push(cursor) ~ "if" ~ "(" ~ Expression ~ ")" ~ Statement ~ optional("else" ~ Statement) ~> ((cur: Int, (e: AExpr), (bt: AStmt), (bf: Option[AStmt])) => AIfStmt(e, bt, bf, cur))
  }

  def Output: Rule1[AStmt] = rule {
    push(cursor) ~ "output" ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AoutputStmt(e, cur))
  }

  def Return: Rule1[AStmt] = rule {
    push(cursor) ~ "return" ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AReturnStmt(e, cur))
  }

  def FunApp: Rule1[AExpr] = rule {
    (push(cursor) ~ Parens ~ FunActualArgs ~> ((cur: Int, fun: AExpr, args: immutable.Seq[AExpr]) => ACallFuncExpr(fun, args, cur)())
     | push(cursor) ~ Identifier ~ FunActualArgs ~> ((cur: Int, id: AIdentifier, args: immutable.Seq[AExpr]) => ACallFuncExpr(id, args, cur)()))
  }

  def FunActualArgs: Rule1[immutable.Seq[AExpr]] = rule {
    "(" ~ zeroOrMore(Expression).separatedBy(",") ~ ")"
  }
}
