package tip.concolic

import org.junit.Test
import org.junit.Assert._
import tip.ast.AProgram
import scala.util.Try
import tip.interpreter.Interpreter
import scala.util.Success
import java.io.ByteArrayInputStream
import scala.io.StdIn
import tip.InterpreterUtils._
import smtlib.parser.Commands.CheckSat
import smtlib.parser.CommandsResponses.CheckSatStatus
import smtlib.parser.Terms.SExpr

import smtlib.parser._
import smtlib.parser.Terms._
import smtlib.parser.Commands._
import smtlib.theories.Ints._
import smtlib.theories.Core._
import smtlib.parser.CommandsResponses._

class SymbolicInterpreterTests {

	@Test
	def haveSameOutput() {
		val files = List("examples/interpreter_test.tip", "examples/ex1.tip", "examples/signs.tip", "examples/apply2.tip", "examples/signs_fun.tip","examples/signs_fun.tip", "tipprograms/map.tip")
		for(file <- files) {
		  val p1 = prepare(file)
  		val p2 = prepare(file)
			val cres = new Interpreter(p1).run()
			val sres = new SymbolicInterpreter(p2).run()
			assertEquals(cres, sres)
		}
	}
	
	/*
	@Test
	def haveSameOutputRequiringInput() {
	  val in = new ByteArrayInputStream(("22\n11").getBytes)
		val file = ("tipprograms/symbolic1.tip")
		val p1 = prepare(file)
		val p2 = prepare(file)
		Console.withIn(in)  {
		  val cres = new Interpreter(p1).run()   
		  assertEquals(cres, 42)
	  }
	  val sres = new SymbolicInterpreter(p2).run(List(22,11))
	  assertEquals(sres, 42)
	}
	*/
	
	

}
