package tip.analysis

import tip.ast.DepthFirstAstVisitor
import tip.ast._
import scala.collection.immutable

/**
 * The analysis associates with each identifier its definition,
 * using the syntactical scope.
 */
case class DeclarationAnalysis(prog: AProgram)
  extends DepthFirstAstVisitor[immutable.Map[String, AIdentifierDeclaration]] {

  visit(prog, Map())

  /**
   * The method recursively visit a the nodes of the AST.
   * An environment env is provided as arguments, associating with each
   * identifier name the node that declares it.
   * Whenever an identifier is visited the declaration associated with its
   * name in the environment is attached to the metadata of the identifier.
   *
   * @param node the node to visit
   * @param env the environment associating with each name its declaration in the current scope
   */
  override def visit(node: AstNode, env: immutable.Map[String, AIdentifierDeclaration]): Unit = {
    node match {
      case block: ABlockStmt =>
        // We visit each statement with the environment extended
        // by the previous declarations in the block
        block.content.foldLeft(env) { (accEnv, stmt) =>
          val extendedEnv = peekDecl(stmt) ++ accEnv
          visit(stmt, extendedEnv)
          extendedEnv
        }
      case funDec: AFunDeclaration =>
        // We associate to each parameter itself as definition
        val argsMap = funDec.args.foldLeft(Map[String, AIdentifierDeclaration]()) { (acc, cur: AIdentifier) =>
          cur.meta.definition = Some(cur)
          acc + (cur.value -> cur)
        }
        // We associate to the function name the function declaration as definition
        funDec.name.meta.definition = Some(funDec)
        val newEnv = env ++ argsMap + (funDec.name.value -> funDec)
        visit(funDec.name, newEnv)
        visit(funDec.stmts, newEnv)
      case p: AProgram =>
        // We assume there can be mutually recursive function
        // So we pre-bind all the functions to their definitions before visiting each of them
        val extended = p.fun.foldLeft(Map[String, AIdentifierDeclaration]()) { (accEnv, fd: AFunDeclaration) =>
          accEnv + (fd.name.value -> fd)
        }
        p.fun.foreach { fd => visit(fd, extended) }
      case varr: AVarStmt =>
        // We associate each x of var x to itself, we don't visit the children
        varr.declIds.foreach { id => id.meta.definition = Some(id) }
      case ident: AIdentifier =>
        // We associate with each identifier the definition in the environment
        try {
          ident.meta.definition = Some(env(ident.value))
        } catch {
          case e: Exception =>
            throw new RuntimeException(s"Error retrieving definition of $ident in ${env.keys}", e)
        }
      case _ =>
        // There is no alteration of the environment,
        // we just visit the children in the current environment
        visitChildren(node, env)
    }
  }

  /**
   * The method returns a map containing the new declarations contained in the statement
   *
   * @param stmt the statement to analyze
   * @return a map associating with each name the node that declares it
   */
  private def peekDecl(stmt: AStmt): immutable.Map[String, AIdentifierDeclaration] = {
    stmt match {
      case dec: AVarStmt =>
        dec.declIds.foldLeft(Map[String, AIdentifierDeclaration]()) { (acc, dec) =>
          //Note this may override (and hide) the previous declaration
          acc + (dec.value -> dec)
        }
      case _ =>
        Map[String, AIdentifierDeclaration]()
    }
  }

}
