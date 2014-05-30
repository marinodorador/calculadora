package strategies

import syntax.AbstractTree._
import scala.collection.mutable.{ListBuffer, Map}
class NumericIntegralStrategy(tree:AbstractTree)  extends CalculatorStrategy{
	abstractTree = tree
	def calculate():AbstractTree = {
	  //Get params
	  val params:ListBuffer[AbstractTree] = abstractTree.asInstanceOf[Function].params;
	  val expresion:AbstractTree = params.apply(0);
	  val variable:Id= params.apply(1).asInstanceOf[Id];
	  var env05:Environment = {case "x" => 0}
	  val a:Double= eval(params.apply(2).asInstanceOf[Vector].params.apply(0),env05);
	  val b:Double= eval(params.apply(2).asInstanceOf[Vector].params.apply(1),env05);
	  val n:Int = 1000000;
	  val h:Double= (b-a)/(2*n);
	  var env01:Environment =  null
	  var env02:Environment = null
	  var env03:Environment = {case variable.id => a}
	  var env04:Environment = {case variable.id => b}
	  var result: Double = 0
	  var sum01: Double = 0
	  var sum02: Double = 0
	  var i = 0
	  for(i <- 1 until n){
	    env01 = {case variable.id => a+2*i*h}
	    val e= eval(expresion,env01)
	    sum01= sum01 + e
	   
	  }
	  for(i <- 1 until n-2){
	    env02 = {case variable.id => a+(2*i)*h}
	    sum02= sum02 + eval(expresion,env02)
	  }
	  
	  result = h/3*(eval(expresion,env03) + eval(expresion,env04) + 2*sum01 +4*sum02)
	  
	  new Const(result)
	}
}