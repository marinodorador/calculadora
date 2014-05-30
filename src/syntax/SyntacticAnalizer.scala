package syntax
import lexic.LexicAnalizer;
import lexic._;
import AbstractTree._;
import scala.collection.mutable.{ListBuffer, Map}

class SyntacticAnalizer(alex:LexicAnalizer) {
	private var currentToken:LexicCategorie = null;
	private var tree:AbstractTree = null;
	//Define table of analysis
	
	def analize():Boolean = {
	  currentToken = alex.getNext;
	  var e_attrs:Map[String, AnyRef] = Map("node" -> null)
	  val result = _e(e_attrs)
	  tree = e_attrs.apply("node").asInstanceOf[AbstractTree]
	  if(result && currentToken.isInstanceOf[Final] ){
		  return true;
	  }
	  false
	}
	
	def getTree():AbstractTree = {
	   this.tree;
	}
	// Analize <E>
	def _e(attrs:Map[String, AnyRef]):Boolean={
	  currentToken match {
	    case (_:Identifier |  _:Opad |  _:Group | _:Number| _:Separator) =>{ //<T><E>’
	      if(currentToken.isInstanceOf[Group] && !currentToken.lexeme.equals("(")  ) return false
	      //term
	      var t_attrs:Map[String, AnyRef] =  Map("node" -> null) //Semantic
	      if(!_t(t_attrs)) return false
	      attrs.update("node", t_attrs.apply("node")) //Semantic
	      //sum
	      var ep_attrs:Map[String, AnyRef] =  Map("node" -> null, "first_t" -> t_attrs.apply("node")  ) //Semantic
	      if(!_ep(ep_attrs)) return false
	      
	      /*Semantic*/
	      if(ep_attrs.apply("node") == null){
	    	attrs.update("node", t_attrs.apply("node"))
	      }else{
	        attrs.update("node", ep_attrs.apply("node"))
	      }
	      /*------------------------------*/
	    }
	    case _ => return false
	  }
	  true;
	} 
	
	//Analize <function_id>
	def _function_id(attrs:Map[String, AnyRef]):Boolean = {
       if(!currentToken.isInstanceOf[Identifier]) return false
       val aux = currentToken 
       currentToken= alex.getNext()
       var params:ListBuffer[AbstractTree] = new ListBuffer[AbstractTree]
       if(!_arguments(params)) return false
       //println("length: "+params.length)
       if(params.length != 0){
         //println(params)
         attrs.update("node", new Function(new Id(aux.lexeme), params))
       }else
         attrs.update("node",  Id(aux.lexeme))
       true
	} 
	
	// Analize <arguments>
	def _arguments(params:ListBuffer[AbstractTree]):Boolean={
	
      if(currentToken.lexeme.equals("(") ) { //“(“ <params>“)”
		  currentToken = alex.getNext();
		  if(!_params(params)) return false
		  if(!currentToken.lexeme.equals(")") ) return false
		  currentToken = alex.getNext();
      }else{
		  currentToken match{
		    case (_:Group|_:Pow|_:Opml|_:Opad|_:Number|_:Separator|_:Final)=>{//Lambda
		    	if(currentToken.isInstanceOf[Group] && 
		    	    (!currentToken.lexeme.equals("}") || 
		    	     !currentToken.lexeme.equals(")")
		    	    )){
		    	  //return false;
		    	}
		    }
		    case _ => return false
		  }
      }
	  
	  true;
	}
	
	//Analize <params>
	def _params(params:ListBuffer[AbstractTree]):Boolean={
	  currentToken match{
	    case (_:Identifier| _:Group| _:Number| _:Opad) => { // <arg><params>’
	      if(currentToken.isInstanceOf[Group] && 
	          !currentToken.lexeme.equals("{")  && 
	          !currentToken.lexeme.equals("[") ) return false
	      var arg_attrs:Map[String, AnyRef] =  Map("node" -> null)
	      if(!_arg(arg_attrs)) return false
	      params.append(arg_attrs.apply("node").asInstanceOf[AbstractTree])
	      if(!_params_prime(params))return false
	    }
	  }
	  true;
	} 
	
	// Analize <params>'
	def _params_prime(params:ListBuffer[AbstractTree]):Boolean={ 
	  if(currentToken.lexeme.equals(")") ){ //lambda
	    //Do nothing
	  }else if(currentToken.lexeme.equals(",")){ //,
	      currentToken = alex.getNext;
	      var arg_attrs:Map[String, AnyRef] =  Map("node" -> null)
	      if(!_arg(arg_attrs)) return false
	      params.append(arg_attrs.apply("node").asInstanceOf[AbstractTree])
	      if(!_params_prime(params))return false
      }else return false
	  true;
	}
	
	//Analize <arg>
	def _arg(attrs: Map[String, AnyRef]):Boolean={
	  if(currentToken.isInstanceOf[Identifier] ){//id
	    attrs.update("node",new Id(currentToken.asInstanceOf[Identifier].lexeme)) /*Semantic*/
	    currentToken = alex.getNext
	  }else if(currentToken.isInstanceOf[Number] ){//number
	    attrs.update("node",new Const(currentToken.asInstanceOf[Number].value)) /*Semantic*/
	    currentToken = alex.getNext
	  }else if(currentToken.lexeme.equals("{") ){ //“{“ <E>“}”
		  currentToken = alex.getNext
		  var e_attrs:Map[String, AnyRef] =  Map("node" -> null)
		  if(!_e(e_attrs))return false
		  attrs.update("node",e_attrs.apply("node")) /*Semantic*/
		  if(!currentToken.lexeme.equals("}")) return false 
		  currentToken = alex.getNext
	  }else if(currentToken.lexeme.equals("[") ){ //vector
		  var vector_attrs:Map[String, AnyRef] =  Map("node" -> null)
		  if(!_vector(vector_attrs)) return false
		  attrs.update("node",vector_attrs.apply("node")) /*Semantic*/
	  }else return false
	  
	  true;
	} 
	
	
	//Analize <E>'
	def _ep(attrs:Map[String, AnyRef]):Boolean={
	  if(currentToken.lexeme.equals(")") 
	      || currentToken.lexeme.equals("}") 
	      || currentToken.lexeme.equals("]") 
	      || currentToken.isInstanceOf[Separator] 
	      || currentToken.isInstanceOf[Final]
	  ){ //lambda
		  //Do nothing
	  }else if(currentToken.isInstanceOf[Opad]){ // opad<T><E>’
	    val aux = currentToken;
	    currentToken = alex.getNext
	     var t_attrs:Map[String, AnyRef] =  Map("node" -> null) /*Semantic*/
	    if(!_t(t_attrs)) return false
	    /*Semantic*/
	    if(attrs.apply("node") == null)
		    if(aux.lexeme.equals("+")){
		      attrs.update("node", new Sum(attrs.apply("first_t").asInstanceOf[AbstractTree],t_attrs.apply("node").asInstanceOf[AbstractTree]))
		    }else 
		      attrs.update("node", new Rest(attrs.apply("first_t").asInstanceOf[AbstractTree],t_attrs.apply("node").asInstanceOf[AbstractTree]))
	    else
	        if(aux.lexeme.equals("+")){
	          attrs.update("node",new Sum(attrs.apply("node").asInstanceOf[AbstractTree],t_attrs.apply("node").asInstanceOf[AbstractTree]))
		    }else{
		      attrs.update("node",new Rest(attrs.apply("node").asInstanceOf[AbstractTree],t_attrs.apply("node").asInstanceOf[AbstractTree]))
		    }
	    /*------------------------*/
	    if(!_ep(attrs))return false
	  }else return false
	  true;
	}
	
	//Analize <T>
	def _t(attrs:Map[String, AnyRef]):Boolean={
	  
	  currentToken match{
	    case(_:Identifier | _:Group|_:Number|_:Opad)=>{//<F><T>’
	      if(currentToken.isInstanceOf[Group] && (!currentToken.lexeme.equals("(")))return false
	      var f_attrs:Map[String, AnyRef] =  Map("node" -> null) /*Semantic*/
	      if(!_f(f_attrs))return false
	      attrs.update("node", f_attrs.apply("node"))  /*Semantic*/
	      var tp_attrs:Map[String, AnyRef] =  Map("node" -> null,"first_factor"->f_attrs.apply("node"))  /*Semantic*/
	      if(!_tp(tp_attrs))return false
	       /*Semantic*/
	      if(tp_attrs.apply("node") == null){
	        attrs.update("node", f_attrs.apply("node"))
	      }else{
	        attrs.update("node", tp_attrs.apply("node"))
	      }
	       /*--------*/
	    }
	    case _ => return false
	  }
	  true;
	}
	
	//Analize <T>'
	def _tp(attrs:Map[String, AnyRef]):Boolean={
	  if(currentToken.isInstanceOf[Opml]){ //opml<F><T>’
	    val aux = currentToken
	    currentToken = alex.getNext;
	    var f_attrs:Map[String, AnyRef] =  Map("node" -> null)  /*Semantic*/
	   if(!_f(f_attrs)) return false;
	     /*Semantic*/
	   if(attrs.apply("node") == null)
	     if(aux.lexeme.equals("*")){
	      attrs.update("node", new Prod(attrs.apply("first_factor").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))
	     }else 
	      attrs.update("node", new Div(attrs.apply("first_factor").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))
	    else
	        if(aux.lexeme.equals("*")){
	          attrs.update("node",new Prod(attrs.apply("node").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))
		    }else{
		      attrs.update("node",new Div(attrs.apply("node").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))
		    }
	    
	     /*-------*/
	    if(!_tp(attrs)) return false;
	  }else if(
	      
	   currentToken.isInstanceOf[Identifier]
	  ||currentToken.isInstanceOf[Number]
	  ||currentToken.lexeme.equals("(")){//<F><T>’
	    var f_attrs:Map[String, AnyRef] =  Map("node" -> null)
	    if(!_f(f_attrs)) return false;
		  	if(attrs.apply("node") == null)
		      attrs.update("node", new Prod(attrs.apply("first_factor").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))
		    else
		      attrs.update("node",new Prod(attrs.apply("node").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))  
	    if(!_tp(attrs)) return false;
	     /*-------*/
	    //check if tp is not a number if it's then there is an error
	  } else if(currentToken.lexeme.equals(")")  //Lambda
	      || currentToken.lexeme.equals("}") 
	      || currentToken.lexeme.equals("]")
	      || currentToken.isInstanceOf[Separator]
	      || currentToken.isInstanceOf[Final]
	      || currentToken.isInstanceOf[Opad] 
	  ){
	    // Do nothing
	  }else return false
	  true;
	}
	
	// Analize <F>
	def _f(attrs:Map[String, AnyRef]):Boolean={
	  currentToken match{
	    case(_:Identifier | _:Group|_:Number|_:Opad)=>{//<B><power>
	    	if(currentToken.isInstanceOf[Group] && (!currentToken.lexeme.equals("(") ))return false
	    	var b_attrs:Map[String, AnyRef] =  Map("node" -> null)    /*Semantic*/
	    	if(!_b(b_attrs)) return false
	    	attrs.update("node", b_attrs.apply("node"))  /*Semantic*/
	    	var power_attrs:Map[String, AnyRef] =  Map("node" -> null,"base"-> b_attrs.apply("node"))    /*Semantic*/
	    	if(!_power(power_attrs))return false
	    	 /*Semantic*/
	    	if(power_attrs.apply("node") != null){
	    		attrs.update("node", power_attrs.apply("node"))
	    	}
	    	 /*-------*/
	    }
	    case _=> return false
	  }
	  true;
	} 
	
	// Analize <B>
	def _b(attrs:Map[String, AnyRef]):Boolean={
	  
	  if(currentToken.isInstanceOf[Identifier]){ //<function_id>
	    var function_attrs:Map[String, AnyRef] =  Map("node" -> null)  
	    if(!_function_id(function_attrs))return false
	    attrs.update("node",function_attrs.apply("node"))  /*Semantic*/
	  }else if(currentToken.lexeme.equals("(") ) { // “(“ <E> “)”
		  currentToken = alex.getNext
		  var e_attrs:Map[String, AnyRef] =  Map("node" -> null)   /*Semantic*/
		  if(!_e(e_attrs))return false
		  attrs.update("node",e_attrs.apply("node"))  /*Semantic*/
		  if(!currentToken.lexeme.equals(")"))return false
		  currentToken = alex.getNext
	  }else if(currentToken.isInstanceOf[Number]) {
	    attrs.update("node", new Const(currentToken.asInstanceOf[Number].value))  /*Semantic*/
	    currentToken = alex.getNext // Number
	  }else if(currentToken.isInstanceOf[Opad]){
	    val aux = currentToken
	    currentToken = alex.getNext
	    if(!_b(attrs)) false
	    if(aux.lexeme.equals("-")){
	      attrs.update("node", new Sign(attrs.apply("node").asInstanceOf[AbstractTree]))  /*Semantic*/
	    }
	  }
	  else return false
	 
	  true;
	} 
	
	//Analize <power>
	def _power(attrs:Map[String, AnyRef]):Boolean={
	  if(currentToken.isInstanceOf[Pow]) {
	    currentToken = alex.getNext
	    var f_attrs:Map[String, AnyRef] =  Map("node" -> null)  /*Semantic*/
	    if(!_f(f_attrs)) return false
	    attrs.update("node", new Power(attrs.apply("base").asInstanceOf[AbstractTree],f_attrs.apply("node").asInstanceOf[AbstractTree]))  /*Semantic*/
	  }else if(
	    currentToken.lexeme.equals("(") 
	    ||currentToken.lexeme.equals(")")
	    ||currentToken.lexeme.equals("}")
	    || currentToken.lexeme.equals("]")
	    || currentToken.isInstanceOf[Identifier]
		|| currentToken.isInstanceOf[Opad]
		|| currentToken.isInstanceOf[Opml]
	    || currentToken.isInstanceOf[Number]
	    || currentToken.isInstanceOf[Separator]
	     || currentToken.isInstanceOf[Final]
	  ){
	      //Do nothing
	    }else return false
	  true;
	}
	
	// Analize <vector>
	def _vector(attrs:Map[String, AnyRef]):Boolean={
	  if(!currentToken.lexeme.equals("[") )return false;
	  currentToken = alex.getNext;
	  var params:ListBuffer[AbstractTree] = new ListBuffer[AbstractTree]  /*Semantic*/
	  if(!_elements(params))return false
	  attrs.update("node", new Vector(params))	   /*Semantic*/
	  if(!currentToken.lexeme.equals("]") )return false;
	  currentToken = alex.getNext;
	  true;
	}
	
	// Analize <elements>
	def _elements(params:ListBuffer[AbstractTree]):Boolean={
	  currentToken match {
	    case(_:Identifier| _:Group| _:Opad | _:Number)=>{ //<element><elements>’
	    	if(currentToken.isInstanceOf[Group] && !currentToken.lexeme.equals("(") )return false
	    	var element_attrs:Map[String, AnyRef] =  Map("node" -> null) /*Semantic*/
	    	if(!_element(element_attrs))return false
	    	params.append(element_attrs.apply("node").asInstanceOf[AbstractTree]) /*Semantic*/
	    	if(!_elements_p(params))return false
	    }
	  }
	  true;
	} 
	
	 // Analize <elements>
	def _elements_p(params:ListBuffer[AbstractTree]):Boolean={
	  if(currentToken.lexeme.equals("]") ){
	    //Do nothing
	
	  }else if(currentToken.isInstanceOf[Separator]){//“,”<element><elements>’
	     
	    currentToken = alex.getNext
	     var element_attrs:Map[String, AnyRef] =  Map("node" -> null) /*Semantic*/
	    if(!_element(element_attrs))return false
	    params.append(element_attrs.apply("node").asInstanceOf[AbstractTree]) /*Semantic*/
	    if(!_elements_p(params)) return false
	  }else return false
	  true;
	}
	
	//Analize <element>
	def _element(attrs:Map[String, AnyRef]):Boolean={
	  currentToken match {
	    case(_:Identifier| _:Group| _:Opad | _:Number)=>{ //<E>
	    	if(currentToken.isInstanceOf[Group] && !currentToken.lexeme.equals("(") )return false
	    	var e_attrs:Map[String, AnyRef] = Map("node" -> null) /*Semantic*/
	    	if(!_e(e_attrs))return false
	    	attrs.update("node", e_attrs.apply("node")) /*Semantic*/
	    }
	  }
	  true;
	} 
	
}