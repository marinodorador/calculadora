package strategies
import syntax.AbstractTree._;

trait CalculatorStrategy{
   type Environment = String => Double
   var abstractTree:AbstractTree = null
   def calculate():AbstractTree
   def eval(t: AbstractTree, env: Environment): Double ={
   //println(t)
     t match {
	  case Sum(l, r)  => {eval(l, env) + eval(r, env)}
	  case Rest(l,r)  => {eval(l, env) - eval(r,env)}
	  case Prod(l,r)  => {eval(l, env) * eval(r,env);}
	  case Div(l,r)   => eval(l,env) / eval(r,env)
	  case Power(l,r) => {Math.pow(eval(l,env),eval(r,env));}
	  case Id(n)      => {env(n)}
	  case Const(v)   => v 
	  case Sign(o)    => {(-1 * eval(o,env))}
	  case Function(i, p) => {
	    i.asInstanceOf[Id].id match{
	      case "sen" => Math.sin(eval(p.apply(0),env))
	      case "cos" => Math.cos(eval(p.apply(0),env)) 
	    }
	  }
    }
   }
}