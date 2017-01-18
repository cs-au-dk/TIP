package tip.ast

import tip.types.TipType

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
  type TypeData = Map[AstNode, Option[TipType]]

  /**
    * Implicitly make declaration data available on identifier AST nodes.
    */
  implicit class AstNodeWithDeclaration(n: AIdentifier)(implicit val data: DeclarationData) {
    def declaration: ADeclaration = data(n)
  }

  /**
    * Implicitly make type data available on AST nodes.
    */
  implicit class AstNodeWithType(n: AstNode)(implicit val data: TypeData) {
    def theType: Option[TipType] = data.getOrElse(n, None)

    private def printer: PartialFunction[AstNode, String] = {
      case id: AIdentifierDeclaration => s"${id.value}: ${id.theType.getOrElse("??")}"
      case f: AFunDeclaration =>
        s"${f.name} (${f.args.map(_.value).mkString(",")}) : ${f.theType.getOrElse("??")}\n${f.stmts.toCustomString(printer)}"
    }

    def toTypedString: String = n.toCustomString(printer)

  }

}
