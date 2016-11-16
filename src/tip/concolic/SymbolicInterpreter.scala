package tip.concolic

import tip.ast._
import tip.logging.Log
import tip.parser.TipParser
import scala.util.Success
import tip.analysis.DeclarationAnalysis
import tip.parser.TipParser
import scala.io.Source
import scala.util.Failure
import tip.logging.Logger
import tip.dot.DotGraph
import tip.dot.DotNode
import tip.dot.DotDirArrow
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.parser.CommandsResponses._
import smtlib.parser.Commands._
import tip.SMTUtils

class SymbolicInterpreter(program: AProgram) {  

  private def pathToSMT(vars : List[Symbol], path: List[(AExpr, Boolean)]) : String = {
    def opToSexp(op : BinaryOperator) : String = {
      op match {
        case Eqq() => "=" 
        case _ => op.toString()
      }
    }
    
    def expToSexp(exp : AExpr) : String = {
      exp match {
        case ABinaryOp(op, left, right, _) => 
          s"(${opToSexp(op)} ${expToSexp(left)} ${expToSexp(right)})" 
        case _ => exp.toString() 
        }
    }
    
    def symbolsToSMT(vars : List[Symbol]) : String = {
      vars.map(sv => s"(declare-const ${sv.value} Int)").mkString("\n")
    }
   
    path.foldLeft(symbolsToSMT(vars))((script : String, cond : (AExpr, Boolean)) => {
      val branchrecord =
        if(cond._2)
           expToSexp(cond._1)
        else 
          "(not " + expToSexp(cond._1) + ")"
      script + "\n" + "(assert " +  branchrecord + ")"
    })
        
  }

  
  
  
//TODO: eliminate options, use separate classes for different types of nodes

  class CTNode(var condition : Option[AExpr] = None, 
             var symcond : Option[AExpr] = None,
             var parent : Option[CTNode] = None) {
  var trueBranch : Option[CTNode] = None
  var falseBranch : Option[CTNode] = None  
  var trueCount : Int = 0
  var falseCount : Int = 0
  var trueUnsat : Boolean = false
  var falseUnsat : Boolean = false


  def children() : List[Option[CTNode]] = {
    List(trueBranch, falseBranch)
  }
  
  override def toString() : String = { 
    condition match { 
      case None => "" 
      case Some(cond) => s"[$cond, $trueCount($trueBranch), $falseCount($falseBranch)]"
    }
  }
  
  def isTrueBranch() : Boolean = {
     parent match {
       case None => false
       case Some(p) => p.trueBranch == Some(this)
     }
  }
  
  def isFalseBranch() : Boolean  = {
    !isTrueBranch()
  }
  
  def pathCondition(suffix : List[(AExpr, Boolean)] = Nil) : List[(AExpr, Boolean)] = {
    this.parent match {
      case None => suffix
      case Some(p) =>
        (p.symcond.get, this.isTrueBranch()) :: p.pathCondition(suffix)
    }
  }
    
  def branch(cond : AExpr, symcond : AExpr, value : Boolean) : CTNode = {
		  condition match {
        case None =>
          log.info(s"Encountered unseen branching condition: $cond")
          this.condition = Some(cond)
          this.symcond = Some(symcond)
        case Some(existing) => 
          log.info(s"Encountered seen branching condition: $cond")
          assert(existing == cond) 
          this.symcond = Some(symcond)
		  }
		  if(value) {
		    trueCount += 1
		    trueBranch match {
		       case None =>
		        log.info(s"Exploring unseen true branch")
		         val newBranch = new CTNode(parent = Some(this)) 
		         trueBranch = Some(newBranch)
		         newBranch
		       case Some(existingBranch) => 
  		        log.info(s"Following explored true branch")
		         existingBranch
		    }
		  } else {
		     falseCount += 1
		     falseBranch match {
		       case None =>
 		        log.info(s"Exploring unseen false branch")
		         val newBranch = new CTNode(parent = Some(this)) 
		         falseBranch = Some(newBranch)
		         newBranch
		       case Some(existingBranch) => 
		         log.info(s"Following explored false branch")
		         existingBranch
		    }
		  }  
  }  
}
  
  val log = Log.typeLogger[this.type](Log.Level.Info)
  
  trait Value
  case class IntValue(i: Int) extends Value
  case class FunValue(fun: AFunDeclaration) extends Value
  case class Location(var i: Option[Value]) extends Value {
    override def equals(obj: scala.Any): Boolean = obj match {
      case rf: AnyRef => this.eq(rf)
      case _ => false
    }
  }
  case object NullValue extends Value
  case class SymbolicValue(value : Value, symbolic : AExpr) extends Value
  case class InterpreterException(message : String, symbolicState : State) extends RuntimeException(message)


  class Symbol(location : Loc, counter : Int) extends AIdentifier("s" + counter.toString, location)()
  
  type Env = Map[AIdentifierDeclaration, Location]
  
    
  trait ExecutionResult {
    val state : State
    def lastNode() : CTNode = {
      state.ct
    }
    def symbolicVars() : List[Symbol] = {
      state.symbols
    }
    def usedInputs() : List[Int] = {
      state.usedInputs
    }
  }
  case class Success(state : State, value: Int) extends ExecutionResult
  case class Failure(state : State, message : String) extends ExecutionResult
  
  
  case class State(var env : Env =  Map[AIdentifierDeclaration, Location](),
                   var symbols : List[Symbol] = Nil,
                   var ct : CTNode = new CTNode(),
                   var counter : Int = 0,
                   var inputs : List[Int] = Nil,
                   var usedInputs : List[Int] = Nil)
  
  
  val returnId = AIdentifier("return", Loc(-1, -1))()
  
  def nextExplorationTarget(lastExplored : CTNode, root : CTNode) : Option[(CTNode, Boolean)] = {
      lastExplored.parent match {
        case None => 
          None
        case Some(parent) => 
           if(parent.trueBranch.isDefined) {
             if(parent.falseBranch.isDefined)
               //true explored, false explored
               nextExplorationTarget(parent, root)
             else {
               if(parent.falseUnsat)
                 //true explored, false unsatisfiable
                 nextExplorationTarget(parent, root)
            	 else 
            	   //true explored, false not explored
            	   Some((parent,false))
             }
           } else { 
             if(parent.trueUnsat) {
               if(parent.falseBranch.isDefined) {
                 //true unsatisfiable, false explored
                 nextExplorationTarget(parent, root)
               } else {
                 if(parent.falseUnsat) 
                   //true unsatisfiable, false unsatisfiable
                   nextExplorationTarget(parent, root)
                 else 
                   //true unsatisfiable, false not explored
                   Some((parent,false))
               }
             }
             else 
               //true not explored
               Some((parent,true))   
           }
      }   
  }
  
  
  def newInputs(symbols : List[Symbol], lastNode : CTNode, root : CTNode) : Option[List[Int]] = {
    val target = nextExplorationTarget(lastNode, root)
    target match {
      case Some((targetNode, value)) => {
        val pc = targetNode.pathCondition(List((targetNode.symcond.get, value)))
        log.info(s"Path condition for next run: $pc")       
        val smt = pathToSMT(symbols, pc)
        log.info(s"SMT script for next run: \n$smt")       
        SMTUtils.solve(smt) match {
          case None => 
            log.info(s"Path condition is unsatisfiable.")
            if(value) 
              targetNode.trueUnsat = true
            else
              targetNode.falseUnsat = true
            return newInputs(symbols, lastNode, root)
          case Some(mapping) =>
            log.info(s"Model: $mapping")
            return Some(symbols.map(v =>
              mapping.get(v.value).map(_.toInt).getOrElse(scala.util.Random.nextInt)))
          }
      }
      case _ => return None
    }
  }

  def test(budget : Int = Int.MaxValue) : Unit =  {
      val root = new CTNode()
      var runs = 0
      var results : List[ExecutionResult] = Nil
      var inputs : List[Int] = Nil
      var symbols : List[Symbol] = Nil
      while(runs <= budget) {
        assert(inputs.length == symbols.length)
        runs += 1
        log.info("\n")
        log.info(s"Starting run $runs")
        val result = run(computationTree = root, inputs)
        results = result :: results
        symbols = result.symbolicVars()
        newInputs(symbols, result.lastNode, root) match {
          case Some(values) => 
            inputs = values
          case None => 
            log.info(s"Finished exhaustive exploration in $runs runs.\n")
            reportExplorationStatistics(results)
            return
        }
      }
      log.info(s"Exhausted search budget after $runs runs.\n")
      reportExplorationStatistics(results)
  }
  
  private def reportExplorationStatistics(results : List[ExecutionResult]) : Unit = {
    val successes = results.collect { case s: Success => s }
    log.info(s"Found ${successes.length} successful input sequences.")
    successes.foreach(s => 
      log.info(s"Input sequence ${s.usedInputs()} produces: \n${s.value}")
      )
    val failures = results.collect { case f: Failure => f }  
    log.info(s"Found ${failures.length} failure-inducing input sequences.")
    failures.foreach(f => 
      log.info(s"Input sequence ${f.usedInputs()} fails with: \n${f.message}."))
  }  
  
  def run(computationTree : CTNode = new CTNode,
          inputs : List[Int] = Nil) : ExecutionResult = {
    log.info(s"Running program with inputs: $inputs")
    try {
      val result = runFunction(program.mainFunction, Seq(), State(ct = computationTree, inputs = inputs))
      result match {
        case (resultState, IntValue(i)) => {
          log.info(s"Program ran succesfully, result: $i")
          Success(resultState, i)
        }
        case (resultState, value) => throw new InterpreterException(s"Illegal main function: returned $value, integer expected.", resultState)
      }
    }
    catch {
      case exception @ InterpreterException(message, errorState) =>
        Failure(errorState, message)
    }
  }

  private def runFunction(fun: AFunDeclaration, args: Seq[Value], s : State): (State, Value) = {    
    val boundEnv = program.fun.foldLeft(Map[AIdentifierDeclaration, Location]()) { (a: Env, f: AFunDeclaration) =>
      a + (f -> new Location(Some(FunValue(f))))
    } ++
      fun.args.zip(args).foldLeft(Map[AIdentifierDeclaration, Location]()) { (a: Env, p: (AIdentifier, Value)) =>
        a + (p._1 -> new Location(Some(p._2)))
      } +
      (returnId -> new Location(None))
      
    val newState = runStatement(fun.stmts, s.copy(env = boundEnv))
    newState.env(returnId).i match {
      case Some(x) => (newState, x)
      case None => missingReturn(fun, newState)
    }
  }
  
  private def runStatement(stm: AStmt, s : State) : State = {
    stm match {
      case AAssignStmt(left: AAssignable, right: AExpr, _) =>
        val (s1, value) = runExpression(right, s)
        left match {
          case id: AIdentifier => {
            s1.env(id.meta.definition.get).i = Some(value)
          }
          case AUnaryOp(DerefOp(), id: AIdentifier, loc) =>
            s1.env(id.meta.definition.get).i match {
              case Some(location @ Location(x)) => location.i = Some(value)
              case Some(NullValue) => nullPointerException(loc, s1)
              case _ => dereferenceNonPointer(loc, s1)
            }
          case _ => throw new RuntimeException(s"Unassignable on the left-hand side of an assignment: $left")
        }
        s1
      
      case ABlockStmt(content, _) => content.foldLeft(s)((s: State, stm: AStmt) => runStatement(stm, s))
      case AIfStmt(guard, ifBranch, elseBranch, loc) =>
        val (s1, value) = runExpression(guard, s)     
        value match {
          case SymbolicValue(concvalue, symbolicexpression) =>  {
            concvalue match {
              case IntValue(0) => {    
                val news = s1.copy(ct = s1.ct.branch(guard, symbolicexpression, false))
                elseBranch.map(stmt => runStatement(stmt, news)).getOrElse(news)
              }
              case IntValue(1) => {
                val news = s1.copy(ct = s1.ct.branch(guard, symbolicexpression, true))
                runStatement(ifBranch, news)
              }
              case _ => guardNotInteger(loc, s1)
            }
          }
          case IntValue(0) =>        
            elseBranch.map(stmt => runStatement(stmt, s1)).getOrElse(s1)
          case IntValue(1) => 
            runStatement(ifBranch, s1)
          case _ => guardNotInteger(loc, s1)
        }
      case ret: AReturnStmt => {
        val (news, res) = runExpression(ret.value, s)
        news.env(returnId).i = Some(res)
        news
      }
      case err : AErrorStmt =>
        val (s1, v) = runExpression(err.value, s)
        v match {
          case IntValue(errorCode) => 
            val msg = s"Application exception occurred during program execution, error code: $errorCode";
            log.info(msg)
            throw new InterpreterException(msg, s1)
          case _ => throw new RuntimeException(s"Error statement expects integer value as error code, given $v.")
       }
      case AVarStmt(ids, _) => {
       ids.foreach { id => s.env = s.env + (id.meta.definition.get -> new Location(None)) }
       s
      }
      case w: AWhileStmt =>
        val (s1, gvalue) = runExpression(w.guard, s)
        gvalue match {
          case IntValue(0) => s1
          case IntValue(_) => runStatement(w, runStatement(w.innerBlock, s1))
          case _ => guardNotInteger(w.offset, s1)
        }
      case AoutputStmt(value, _) =>
        val (s1, out) = runExpression(value, s)
        out match {
          case IntValue(x) => log.info(s"Program out: $x"); s1
          case _ => throw new InterpreterException(s"Output not supported for non-integer values", s1)
        }
    }
  }

  private def solverSupportedOperator(op : BinaryOperator) : Boolean = {
    op match {
      case Divide() => false
      case Times() => false
      case _ => true
    }
  }
  
  private def runExpression(exp: AExpr, s : State): (State, Value) = {
    val location = exp.offset;
    exp match {
      case e: ABinaryOp => 
        val (s1, left) = runExpression(e.left, s)
        val (s2, right) = runExpression(e.right, s1)
        e.operator match {
          case Eqq() => 
            def integerEquality(lv : Int, rv : Int) : IntValue = 
              IntValue(if (lv == rv) 1 else 0)
            def symbolicEquality(lsym : AExpr, rsym : AExpr) : ABinaryOp =
              ABinaryOp(Eqq(), lsym, rsym, location)()

            (s2, (left, right) match {    
              case (IntValue(lv), IntValue(rv)) => 
                integerEquality(lv,rv)
              case (IntValue(lv), SymbolicValue(IntValue(rv), rsym)) =>
                new SymbolicValue(integerEquality(lv,rv), symbolicEquality(new ANumber(lv, location)(), rsym))
              case (SymbolicValue(IntValue(lv), lsym), IntValue(rv)) =>
                new SymbolicValue(integerEquality(lv,rv), symbolicEquality(lsym, new ANumber(rv, location)()))
              case (SymbolicValue(IntValue(lv), lsym), SymbolicValue(IntValue(rv), rsym)) =>
                new SymbolicValue(integerEquality(lv,rv), symbolicEquality(lsym, rsym))
              case _  =>
                IntValue(if (left == right) 1 else 0)
                
            })
          case op: Operator => 
            def arithmetic(lv : Int, rv : Int) : IntValue = 
              op match {
                case Divide() => IntValue(lv / rv)
                case GreatThan() => IntValue(if (lv > rv) 1 else 0)
                case Minus() => IntValue(lv - rv)
                case Plus() => IntValue(lv + rv)
                case Times() => IntValue(lv * rv)
                case _ => ???
              }
            
            def symbolicArithmetic(lsym : AExpr, rsym : AExpr) : ABinaryOp =
              ABinaryOp(op, lsym, rsym, location)()
              
            val value = (left, right) match {
                   case (IntValue(lv), IntValue(rv)) =>
                     arithmetic(lv,rv)
                   case (IntValue(lv), SymbolicValue(IntValue(rv), rsym)) => 
                     new SymbolicValue(arithmetic(lv,rv), symbolicArithmetic(new ANumber(lv, location)(), rsym))
                   case (SymbolicValue(IntValue(lv), lsym), IntValue(rv)) => {
                     val symbolic = symbolicArithmetic(lsym, new ANumber(rv, location)())
                     new SymbolicValue(arithmetic(lv,rv), symbolic)
                   }
                   case (SymbolicValue(IntValue(lv), lsym), SymbolicValue(IntValue(rv), rsym)) => 
                     if(solverSupportedOperator(op))
                       new SymbolicValue(arithmetic(lv,rv), symbolicArithmetic(lsym,rsym))
                     else {
                       log.info(s"Substituting run-time value ($rv) for right operand in symbolic expression (${symbolicArithmetic(lsym,rsym)}).")
                       new SymbolicValue(arithmetic(lv,rv), symbolicArithmetic(lsym,new ANumber(rv, location)()))
                     }
                   case _ => 
                     throw new RuntimeException(s"Unable to apply the operator $op to non-integer values")
            };
            (s2, value)
         }
      case id: AIdentifier =>
        val definition = id.meta.definition.get
        s.env(definition).i match {
          case Some(z) => (s,z)
          case None => throw new RuntimeException(s"Uninitialised variable at ${id.offset}")
      }
      case AInput(_) => {
        val newSymbol = new Symbol(exp.offset,s.counter)
        val (num, newInputs) = s.inputs.headOption match {
          case Some(value) => (value, s.inputs.tail)
          case _ => (scala.util.Random.nextInt, Nil)
        }
        log.info(s"Using value $num for $newSymbol.")
        val s1 = s.copy(symbols = s.symbols ::: List(newSymbol),
                        counter = s.counter + 1,
                        inputs = newInputs,
                        usedInputs = s.usedInputs ::: List(num))
        (s1, new SymbolicValue(new IntValue(num), newSymbol))      
      }
      
      
      case AMalloc(_) => (s, new Location(None))
      case ANull(_) => (s, NullValue)
      case ANumber(value, _) => (s, IntValue(value))
      case AUnaryOp(op: DerefOp, target: AExpr, loc) =>
        runExpression(target, s) match {
          case (s2, Location(Some(x))) => (s2, x)
          case (s2, NullValue) => nullPointerException(loc, s2)
          case (s2, _) => dereferenceNonPointer(loc, s2)
        }
      case AUnaryOp(op: RefOp, target: AExpr, loc) =>
        target match {
          case id: AIdentifier => (s, s.env(id.meta.definition.get))
          case _ => throw new RuntimeException(s"Cannot take the reference of an expression at $loc")
        }
      case ACallFuncExpr(target, args, loc) =>
        val (s2, funValue) = runExpression(target, s)
        funValue match {
          case f: FunValue => {
            val initial : (List[Value], State) = (Nil,s2)
            val (vals , news) = args.foldLeft(initial)((sofar : (List[Value], State), arg : AExpr) => {
              val (vs, state) = sofar 
              val (newstate, value) = runExpression(arg, state)
              (value :: vs, newstate)
            })
            
            val (afterState, afterValue) = runFunction(f.fun, vals.reverse, news)
            //reset env, keep changes to symbolic state
            afterState.env = news.env
            (afterState, afterValue)
          }
          case _ => throw new RuntimeException(s"Call to a non-function at $loc, $funValue found")
        }
    }
  }

  def missingReturn(fun: AFunDeclaration, s : State) = throw new InterpreterException(s"Missing return statement in ${fun.name}.", s)
  def nullPointerException(loc: Loc, s : State) = throw new InterpreterException(s"NullPointer exception at $loc.", s)
  def guardNotInteger(loc: Loc, s : State) = throw new InterpreterException(s"Guard in $loc not evaluating to an integer.", s)
  def dereferenceNonPointer(loc: Loc, s : State) = throw new InterpreterException(s"Dereferencing a non-pointer at $loc.", s)

}



