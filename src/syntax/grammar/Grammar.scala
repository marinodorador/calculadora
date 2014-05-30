package syntax.grammar
import scala.collection.mutable.ListBuffer

class Rule(var rule:Array[String])

class Production(var p:String, var rules:Array[Rule], var isStart:Boolean)

class Grammar(S:String, P:Array[Production], T:Array[String], N:Array[String]){
	def followings(p: Production){
	  var followings:ListBuffer[String] = new ListBuffer[String];
	  
	  //if is start production then $ is include in.
	  if(p.isStart){
	    followings.append("#")
	  }
	  var px = null;
	  for(px <- P){
		  var rule = null;
		  var betha: ListBuffer[String] = new  ListBuffer[String]();
		  for(rule <- px.rules){
		    var rulStrs = null;
		    for(rulStrs <- rule.rule){
		       if(rulStrs.equals(p.p)){ // If equals get beta
		    	   betha append rulStrs;
		       }
		    }
		    var _betha = betha.toArray
		    if(_betha.length == 0 || this.anulable(new Rule(_betha))){
		      
		    }else{
		      
		    }
		  }
	  }
	  //if(anulable(p.rule)){
	    
	  //}
	  
	}
	
	def first(r: Rule){
		var followings:ListBuffer[String] = new ListBuffer[String];
		if(T.contains(r.rule(0))){
		  
		}
	}
	
	def getProduction(name:String):Production = {
	  var i = null;
	   for(i <- P){
	     if(i.p.equals(name))
	       return i;
	   }
	   null
	}
	
	def anulable(r:Rule):Boolean = {
	  
		if(r.rule(0).equals("$")){
			//println(r.rule(0) + " == '$' .... "+(r.rule(0) == "$"))
			return true;
		}else{
			if(T.contains(r.rule(0))){ // Terminal
			  return false;
			}else{
		
			  if(N.contains(r.rule(0))){ //No terminal
			    val prod:Production = getProduction(r.rule(0))
			    var _r = 0
			    println("prod: "+r.rule(0))
			    for(_r <- prod.rules){ // If exists _r such proc -> _r and anulable(r)
			      println(_r.rule.mkString(",")+": Anulable ... "+anulable(_r))
			      if(anulable(_r)){
			        
			        //Calculate betha
			        var betha = new Array[String](r.rule.length-1);
			        var i = 0;
			        for(i <- 0 until betha.length-1){
			          betha(i) = r.rule(i+1);
			        }
			        if(betha.length == 0) return true;
			       //println("betha"+betha)
			        return anulable(new Rule(betha))
			      }
			    }
			    return false
			  }
			}
		}
				  
		//println("entra1")
		return false
	}
}