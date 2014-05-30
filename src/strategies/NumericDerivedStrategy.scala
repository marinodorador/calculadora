package strategies
import syntax.AbstractTree._
import scala.collection.mutable.{ListBuffer, Map}
class NumericDerivedStrategy(tree:AbstractTree) extends CalculatorStrategy {
	abstractTree = tree
	def calculate():AbstractTree = {
	  //Get params
	  val params:ListBuffer[AbstractTree] = abstractTree.asInstanceOf[Function].params;
	  val expresion:AbstractTree = params.apply(0);
	  val variable:Id= params.apply(1).asInstanceOf[Id];
	  val point:Const= params.apply(2).asInstanceOf[Const];
	  val h:Double = .000005;
	  val env01:Environment =  { case variable.id => (point.v)}//f(x)
	  val env02:Environment =  { case variable.id => (point.v + h)}//f(x) + h
	  val env03:Environment =  { case variable.id => (point.v + 2*h)}//f(x)+2h
	  val env04:Environment =  { case variable.id => (point.v + 3*h)}//f(x)+3h
	  val env05:Environment =  { case variable.id => (point.v + 4*h)}//f(x)+4h
	 var result:Double = 
	    (
	    -25*eval(expresion,env01) +
	    48*eval(expresion,env02) -
	    36*eval(expresion,env03) +
	    16*eval(expresion,env04)) -
	    3*eval(expresion,env05)
	    result = result / (12*h)
	  new Const(result)
	}
	
}