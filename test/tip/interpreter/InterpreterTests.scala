package tip.interpreter

import tip.ast.AProgram
import tip.parser.TipParser
import tip.analysis.DeclarationAnalysis
import scala.util.Success
import scala.io.Source
import org.junit.Test
import org.junit.Assert._
import java.io.ByteArrayInputStream
  
class InterpreterTests {

	def prepare(fileName : String) : AProgram =  {
			val src = Source.fromFile(fileName).mkString
		  val tipParser = new TipParser(src)
			val p = tipParser.InputLine.run()
			p match {
			  case Success(programNode : tip.ast.AProgram) => {
				  //required for side effects on environment
				  new DeclarationAnalysis(programNode)
				  programNode
			  }
			}
	}
	
	@Test
	def factorialRecursiveIterative() {
	  val in = new ByteArrayInputStream(("5\n4").getBytes)
		val recursive = "tipprograms/factorial_iterative.tip"
		val iterative = "tipprograms/factorial_recursive.tip"
		val progrec = prepare(recursive)
		val progite = prepare(iterative)
		Console.withIn(in)  {
		  val resrec = new Interpreter(progrec).run()   
		  assertEquals(resrec, 120)
		  val resite = new Interpreter(progite).run()
		  assertEquals(resite, 24)
	  }
	}
	
	@Test
	def pointerManipulation() {
	  		val p = prepare("tipprograms/pointers.tip")
	  		assertEquals(1, new Interpreter(p).run())
	}
	
	@Test
	def map() {
	  		val p = prepare("tipprograms/map.tip")
	  		assertEquals(42, new Interpreter(p).run())
	}
	
}