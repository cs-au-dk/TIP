package tip.ast

import tip.ast.AstPrinters._
import tip.types.Type

object AstNodeData {

  /**
    * Map from identifier node to corresponding declaration node.
    * @see [[tip.analysis.DeclarationAnalysis]]
    */
  type DeclarationData = Map[AIdentifier, ADeclaration]

  /**
    * Map from AST node to type, if available.
    * @see [[tip.analysis.TypeAnalysis]]
    */
  type TypeData = Map[AstNode, Option[Type]]

  /**
    * Implicitly make declaration data available on identifier AST nodes.
    *
    * To novice Scala programmers:
    * This "implicit class" has the effect that every instance of `AIdentifier` effectively
    * gets an extra field `declaration` (provided that this class has been imported).
    * Note that the value of the field is obtained via the implicit parameter `data`.
    * For more information about implicit classes in Scala, see [[https://docs.scala-lang.org/overviews/core/implicit-classes.html]].
    */
  implicit class AstNodeWithDeclaration(n: AIdentifier)(implicit val data: DeclarationData) {
    def declaration: ADeclaration = data(n)
  }

  /**
    * Implicitly make type data available on AST nodes.
    *
    * (For information about Scala's implicit classes, see [[tip.ast.AstNodeData.AstNodeWithDeclaration]].)
    */
  implicit class AstNodeWithType(n: AstNode)(implicit val data: TypeData) {
    def theType: Option[Type] = data.getOrElse(n, None)

    private def printer: PartialFunction[AstNode, String] = {
      case id: AIdentifierDeclaration => s"${id.name}: ${id.theType.getOrElse("??")}"
      case f: AFunDeclaration =>
        s"${f.name}(${f.params.map(_.name).mkString(",")}): ${f.theType.getOrElse("??")}\n${f.stmts.print(printer)}"
    }

    def toTypedString: String = n.print(printer)

  }

}
