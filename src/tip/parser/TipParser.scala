package tip.parser

import org.parboiled2._
import tip.ast._

import scala.collection._
import scala.language.implicitConversions

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

  object LanguageKeywords {
    val KALLOC = "alloc"
    val KINPUT = "input"
    val KWHILE = "while"
    val KIF = "if"
    val KELSE = "else"
    val KVAR = "var"
    val KRETURN = "return"
    val KNULL = "null"
    val KOUTPUT = "output"
    val KERROR = "error"
  }

  val keywords = Set(
    LanguageKeywords.KALLOC,
    LanguageKeywords.KINPUT,
    LanguageKeywords.KWHILE,
    LanguageKeywords.KIF,
    LanguageKeywords.KELSE,
    LanguageKeywords.KVAR,
    LanguageKeywords.KRETURN,
    LanguageKeywords.KNULL,
    LanguageKeywords.KOUTPUT,
    LanguageKeywords.KERROR
  )

  def InputLine = rule {
    Program ~ EOI
  }

  def WS = rule {
    NewLine | CharPredicate(" \t\f") | Comment
  }

  def OptSpace = rule {
    quiet(zeroOrMore(WS))
  }

  implicit def wspStr(s: String): Rule0 = rule {
    quiet(OptSpace ~ str(s) ~ OptSpace)
  }

  def AssignableExpression: Rule1[Assignable] = rule {
    (DirectFieldWrite | IndirectFieldWrite | Identifier | DerefWrite) ~> { x: Assignable =>
      x
    }
  }

  def DirectFieldWrite: Rule1[ADirectFieldWrite] = rule {
    push(cursor) ~ Identifier ~ wspStr(".") ~ Identifier ~> ((cur: Int, id: AIdentifier, field: AIdentifier) => ADirectFieldWrite(id, field.name, cur))
  }

  def IndirectFieldWrite: Rule1[AIndirectFieldWrite] = rule {
    push(cursor) ~ wspStr("(") ~ wspStr("*") ~ Expression ~ wspStr(")") ~ wspStr(".") ~ Identifier ~> (
      (
        cur: Int,
        exp: AExpr,
        field: AIdentifier
      ) => AIndirectFieldWrite(exp, field.name, cur)
    )
  }

  def DerefWrite: Rule1[ADerefWrite] = rule {
    push(cursor) ~ wspStr("*") ~ Expression ~> ((cur: Int, exp: AExpr) => ADerefWrite(exp, cur))
  }

  def Expression: Rule1[AExpr] = rule {
    Operation ~
      optional(
        push(cursor) ~ ">" ~ Operation ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(GreatThan, e1, e2, cur))
          | push(cursor) ~ "==" ~ Operation ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Eqq, e1, e2, cur))
      )
  }

  def Record: Rule1[ARecord] = rule {
    push(cursor) ~ "{" ~ zeroOrMore(Field).separatedBy(wspStr(",")) ~ "}" ~> ((cur: Int, fields: Seq[ARecordField]) => ARecord(fields.toList, cur))
  }

  def Access: Rule1[AFieldAccess] =
    rule {
      (Identifier | DeRef | Parens) ~ oneOrMore("." ~ Identifier) ~> (
        (
          e1: AExpr,
          fs: Seq[AIdentifier]
        ) => fs.foldLeft(e1)((e: AExpr, f: AIdentifier) => AFieldAccess(e, f.name, f.loc))
      )
    }.asInstanceOf[Rule1[AFieldAccess]]

  def Field: Rule1[ARecordField] = rule {
    push(cursor) ~ Identifier ~ wspStr(":") ~ Expression ~> ((cur: Int, id: AIdentifier, expr: AExpr) => ARecordField(id.name, expr, id.loc))
  }

  def Operation: Rule1[AExpr] = rule {
    Term ~
      optional(
        push(cursor) ~ "+" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Plus, e1, e2, cur))
          | push(cursor) ~ "-" ~ Expression ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Minus, e1, e2, cur))
      )
  }

  def Term: Rule1[AExpr] = rule {
    Access | (Atom ~ optional(
      push(cursor) ~ "*" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Times, e1, e2, cur))
        | push(cursor) ~ "/" ~ Term ~> ((e1: AExpr, cur: Int, e2: AExpr) => ABinaryOp(Divide, e1, e2, cur))
    ))
  }

  def Atom: Rule1[AExpr] = rule {
    (FunApp
      | Number
      | Parens
      | PointersExpression
      | push(cursor) ~ wspStr(LanguageKeywords.KINPUT) ~> ((cur: Int) => AInput(cur))
      | Identifier
      | Record)
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

  private var c: Int = _

  def Id: Rule1[String] = rule {
    run(c = cursor) ~ atomic(capture((CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.AlphaNum | '_')).named("identifier")) ~ quiet(
      test(!keywords.contains(input.sliceString(c, cursor)))
    )
  }

  def Identifier: Rule1[AIdentifier] = rule {
    push(cursor) ~ OptSpace ~ Id ~> ((cur: Int, id: String) => AIdentifier(id, cur))
  }

  def IdentifierDeclaration: Rule1[AIdentifierDeclaration] = rule {
    push(cursor) ~ OptSpace ~ Id ~> ((cur: Int, id: String) => AIdentifierDeclaration(id, cur))
  }

  def PointersExpression = rule {
    (push(cursor) ~ wspStr(LanguageKeywords.KALLOC) ~ Expression ~> ((cur: Int, exp: AExpr) => AAlloc(exp, cur))
      | push(cursor) ~ wspStr("null") ~> ((cur: Int) => ANull(cur))
      | Ref
      | DeRef)
  }

  def Ref = rule {
    push(cursor) ~ "&" ~ Identifier ~> ((cur: Int, id: AIdentifier) => AVarRef(id, cur))
  }

  def DeRef: Rule1[AUnaryOp] = rule {
    push(cursor) ~ "*" ~ Atom ~> ((cur: Int, e: AExpr) => AUnaryOp(DerefOp, e, cur))
  }

  def Program: Rule1[AProgram] = rule {
    push(cursor) ~ zeroOrMore(TipFunction) ~> ((cur: Int, funList: Seq[AFunDeclaration]) => AProgram(funList.toList, cur))
  }

  def TipFunction: Rule1[AFunDeclaration] = rule {
    push(cursor) ~ Identifier ~ "(" ~ zeroOrMore(IdentifierDeclaration).separatedBy(",") ~ ")" ~ FunBlock ~> {
      (cur: Int, id: AIdentifier, args: Seq[AIdentifierDeclaration], b: AFunBlockStmt) =>
        AFunDeclaration(id.name, args.toList, b, cur)
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
    push(cursor) ~ AssignableExpression ~ "=" ~ Expression ~ ";" ~> { (cur: Int, e1: Assignable, e2: AExpr) =>
      AAssignStmt(e1, e2, cur)
    }
  }

  def Block: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ "{" ~ Statements ~ "}" ~> ((cur: Int, body: Seq[AStmtInNestedBlock]) => ANestedBlockStmt(body.toList, cur))
  }

  def FunBlock: Rule1[AFunBlockStmt] = rule {
    push(cursor) ~ "{" ~ VarStatements ~ Statements ~ Return ~ "}" ~> (
      (
        cur: Int,
        declarations: Seq[AVarStmt],
        others: Seq[AStmtInNestedBlock],
        ret: AReturnStmt
      ) => AFunBlockStmt(declarations.toList, others.toList, ret, cur)
    )
  }

  def Declaration: Rule1[AVarStmt] = rule {
    push(cursor) ~ LanguageKeywords.KVAR ~ oneOrMore(IdentifierDeclaration)
      .separatedBy(",") ~ ";" ~> ((cur: Int, idSeq: Seq[AIdentifierDeclaration]) => AVarStmt(idSeq.toList, cur))
  }

  def While: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ LanguageKeywords.KWHILE ~ "(" ~ Expression ~ ")" ~ Statement ~> ((cur: Int, e: AExpr, b: AStmtInNestedBlock) => AWhileStmt(e, b, cur))
  }

  def If: Rule1[AStmtInNestedBlock] = rule {
    (push(cursor) ~ LanguageKeywords.KIF ~ "(" ~ Expression ~ ")" ~ Statement ~ optional(LanguageKeywords.KELSE ~ Statement)) ~> {
      (cur: Int, e: AExpr, bt: AStmtInNestedBlock, bf: Option[AStmtInNestedBlock]) =>
        {
          AIfStmt(e, bt, bf, cur)
        }
    }
  }

  def Output: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ LanguageKeywords.KOUTPUT ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AOutputStmt(e, cur))
  }

  def Return: Rule1[AReturnStmt] = rule {
    push(cursor) ~ LanguageKeywords.KRETURN ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AReturnStmt(e, cur))
  }

  def Error: Rule1[AStmtInNestedBlock] = rule {
    push(cursor) ~ LanguageKeywords.KERROR ~ Expression ~ ";" ~> ((cur: Int, e: AExpr) => AErrorStmt(e, cur))
  }

  def FunApp: Rule1[AExpr] = rule {
    push(cursor) ~ (Parens | Identifier) ~ FunActualArgs ~> ((cur: Int, fun: AExpr, args: Seq[AExpr]) => ACallFuncExpr(fun, args.toList, cur))
  }

  def FunActualArgs: Rule1[Seq[AExpr]] = rule {
    "(" ~ zeroOrMore(Expression).separatedBy(",") ~ ")"
  }
}
