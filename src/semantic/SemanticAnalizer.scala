package semantic
import strategies._;
import syntax.AbstractTree._;

class SemanticAnalizer {
    
	var iStrategy:CalculatorStrategy = null;
	var abstractTree:AbstractTree = null;
	
	def this(abstractTree:AbstractTree){
     this()
     this.abstractTree = abstractTree
   }
	
	def analize():Boolean = {
	  abstractTree match  {
	    case (_:Sum | _:Rest | _:Prod | _:Power | _:Div) => {
	      iStrategy = new EvalStrategy(abstractTree)
	      return true
	    }
	    case Function(id, params) =>{
	      id.asInstanceOf[Id].id match{
	        case "derive" =>{
	          if(params.apply(0).isInstanceOf[AbstractTree] && params.apply(1).isInstanceOf[Id]){
	              if(params.apply(2).isInstanceOf[Const])
	                iStrategy = new NumericDerivedStrategy(abstractTree)
	                return true
	          }else{
	            //error
	          }
	        }
	        case "integra"=>{
	          if(params.apply(0).isInstanceOf[AbstractTree] && params.apply(1).isInstanceOf[Id]){
	              if(params.apply(2).isInstanceOf[Vector])
	                iStrategy = new NumericIntegralStrategy(abstractTree)
	                return true
	          }else{
	            //error
	          }
	        }
	        case _ => {
	          //error
	        }
	      }
	    } 
	  }
	  true
	}
	def getStrategy():CalculatorStrategy = {
	  iStrategy
	}
}