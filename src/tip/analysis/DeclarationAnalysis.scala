package tip.analysis

import tip.ast.DepthFirstAstVisitor
import tip.ast._
import tip.util.TipProgramException

/**
  * Declaration analysis, binds identifiers to their declarations.
  *
  * @see [[tip.ast.AstNodeData]]
  */
class DeclarationAnalysis(prog: AProgram) extends DepthFirstAstVisitor[Map[String, ADeclaration]] with Analysis[AstNodeData.DeclarationData] {

  private var declResult: AstNodeData.DeclarationData = Map()

  /**
    * @inheritdoc
    */
  def analyze(): AstNodeData.DeclarationData = {
    visit(prog, Map())
    declResult
  }

  /**
    * Recursively visits the nodes of the AST.
    * An environment `env` is provided as argument, mapping each identifier name to the node that declares it.
    * Whenever an identifier is visited, `declResult` is extended accordingly.
    *
    * @param node the node to visit
    * @param env the environment associating with each name its declaration in the current scope
    */
  def visit(node: AstNode, env: Map[String, ADeclaration]): Unit =
    node match {
      case block: ABlock =>
        // Extend the environment with the initial declarations in the block, if present
        val ext = block match {
          case fblock: AFunBlockStmt => peekDecl(fblock.declarations)
          case _: ANestedBlockStmt => Map[String, ADeclaration]()
        }
        // Extend the env
        val extendedEnv = extendEnv(env, ext)
        // Visit each statement in the extended environment
        block.body.foreach { stmt =>
          visit(stmt, extendedEnv)
        }
      case funDec: AFunDeclaration =>
        // Associate to each parameter itself as definition
        val argsMap = funDec.params.foldLeft(Map[String, ADeclaration]()) { (acc, cur: AIdentifierDeclaration) =>
          extendEnv(acc, cur.name -> cur)
        }
        // Visit the function body in the extended environment
        val extendedEnv = extendEnv(env, argsMap)
        visit(funDec.stmts, extendedEnv)
      case p: AProgram =>
        // There can be mutually recursive functions, so pre-bind all the functions to their definitions before visiting each of them
        val extended = p.funs.foldLeft(Map[String, ADeclaration]()) { (accEnv, fd: AFunDeclaration) =>
          extendEnv(accEnv, fd.name -> fd)
        }
        p.funs.foreach { fd =>
          visit(fd, extended)
        }
      case ident @ AIdentifier(name, loc) =>
        // Associate with each identifier the definition in the environment
        try {
          declResult += ident -> env(name)
        } catch {
          case _: Exception =>
            throw new DeclarationError(s"Identifier ${ident.name} not declared ${loc.toStringLong}")
        }
      case AAssignStmt(id: AIdentifier, _, loc) =>
        if (env.contains(id.name)) {
          env(id.name) match {
            case f: AFunDeclaration =>
              throw new DeclarationError(s"Function $f cannot appear on the left-hand side of an assignment ${loc.toStringLong}")
            case _ =>
          }
        }
        visitChildren(node, env)
      case AVarRef(id, loc) =>
        if (env.contains(id.name) && env(id.name).isInstanceOf[AFunDeclaration])
          throw new DeclarationError(s"Cannot take address of function ${env(id.name)} ${loc.toStringLong}")
        visitChildren(node, env)
      case ARecord(fields, _) =>
        fields.foldLeft(Set[String]())((s, f) => {
          if (s.contains(f.field))
            throw new DeclarationError(s"Duplicate field name ${f.field} ${f.loc.toStringLong}")
          s + f.field
        })
        visitChildren(node, env)
      case _ =>
        // There is no alteration of the environment, just visit the children in the current environment
        visitChildren(node, env)
    }

  /**
    * Extend the environment `env` with the bindings in `ext`, checking that no re-definitions occur.
    * @param env the environment to extend
    * @param ext the bindings to add
    * @return the extended environment if no conflict occurs, throws a DeclarationError otherwise
    */
  def extendEnv(env: Map[String, ADeclaration], ext: Map[String, ADeclaration]): Map[String, ADeclaration] =
    ext.foldLeft(env)((e, p) => extendEnv(e, p))

  /**
    * Extend the environment `env` with the binding `pair`, checking that no re-definition occurs.
    * @param env the environment to extend
    * @param pair the binding to add
    * @return the extended environment if no conflict occurs, throws a DeclarationError otherwise
    */
  def extendEnv(env: Map[String, ADeclaration], pair: (String, ADeclaration)): Map[String, ADeclaration] = {
    if (env.contains(pair._1))
      throw new DeclarationError(s"Redefinition of identifier ${pair._1} ${pair._2.loc.toStringLong}")
    env + pair
  }

  /**
    * Returns a map containing the new declarations contained in the given sequence of variable declaration statements.
    * If a variable is re-defined, a DeclarationError is thrown.
    * @param decls the sequence of variable declaration statements
    * @return a map associating with each name the node that declares it
    */
  private def peekDecl(decls: Seq[AVarStmt]): Map[String, ADeclaration] = {
    val allDecls = decls.flatMap(v => v.declIds.map(id => id.name -> id))
    allDecls.foldLeft(Map[String, ADeclaration]()) { (map, pair) =>
      extendEnv(map, pair)
    }
  }
}

class DeclarationError(message: String) extends TipProgramException(s"Symbol error: $message")
