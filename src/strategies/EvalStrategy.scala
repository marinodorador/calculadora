package strategies
import syntax.AbstractTree._
import scala.collection.mutable.{ListBuffer, Map}

class EvalStrategy(tree:AbstractTree) extends CalculatorStrategy{
	abstractTree = tree
	
	def calculate():AbstractTree = {
	  val env01:Environment =  {case x => 2};
	  val result = eval(abstractTree,env01);
	  return new Const(result)
	}
}