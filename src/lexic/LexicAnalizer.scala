package lexic
import scala.collection.mutable.Stack;
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import java.util.regex.Pattern
// import following package
import scala.util.control._
import syntax._
import syntax.AbstractTree._


class LexicalException(message: String = null, cause: Throwable = null) extends RuntimeException

class LexicAnalizer(i: String) {
  private def acceptanceState: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7,9)
  private var input: String = i
  private var currentState: Int = 0
  private var readedChars: Stack[Char] = new Stack[Char]()
  private var visitedStates: Stack[Int] = new Stack[Int]()
  var tokens: ListBuffer[LexicCategorie] = ListBuffer[LexicCategorie]()
  private var currentToken = 0;
  def analize() {
    // create a Breaks object as follows
    val loop = new Breaks;
    var index = 0
    var currentChar: Char = 0
    //Leer caracter por caracter
    while (index <= input.length()) {
      try { 
         if(index == input.length()){
          currentChar='\0'; //Si estamos al final de la cadena de caracteres el caracter actual es el final de cadena
        }else{
          currentChar = input.charAt(index)
        }
        readedChars.push(currentChar) //Metemos a la pila el caracter actual
        visitedStates.push(currentState) // Metemos a la pila el estado actual
        nextState(currentChar); //Pasar al siguiente estado
       
      } catch {
        case e: LexicalException => {
            var error=true; //Partimos del hecho de que hay un error.
        	loop.breakable{
	            while (visitedStates.length > 0) { //Revisamos los estados visitados en busca de un estado de aceptación
	              currentState = visitedStates.pop // Sacamos el último estado
	              readedChars.pop // Sacamos el ultimo caracter
	              index-=1;
	              /* Si encontramos un estado de aceptación hemos encontrado un lexema
	               *y lo evaluamos (para asignarle la categoria lexica que le corresponde) 
	               *  finalmente y lo agremos a lista.
	               * */
	              if (acceptanceState.contains(currentState)) { 
	                var lexeme: String = readedChars.toArray.mkString("").reverse 
	                evalLexeme(lexeme)
	                readedChars.clear //limpiamos la pila de caracteres leidos
	                visitedStates.clear // Limpiar pila de estados
	                currentState = 0; // Comenzamos de nuevo en el estado cero, pero ahora en el último caracter leido.
	                error = false;
	                loop.break
	              }
	            }
        	}
          if(error) throw new LexicalException("You have a lexical exception...");
        }
      }
      index += 1
    }
     //Agrega un categoria lexica que indica el final de la lista.
     tokens.append(new Final("#"))
  }
  private def evalLexeme(lexeme: String) {
    currentState match {
      case 1 => { //Integer
        //Calculate value
        val value = lexeme.toDouble
        tokens append  new Number(lexeme,  value)
      }
      case 2 => { //Real
        //Calculate value
        val value = lexeme.toDouble
        tokens append new Number(lexeme, value)
      }
      case 3 => tokens append  new Group(lexeme)
      case 4 => tokens append  new Identifier(lexeme)
      case 5 => {
        if (lexeme.equals("^"))
        	tokens append  new Pow(lexeme)
        else if  (lexeme.equals("*") || lexeme.equals("/"))
        	tokens append  new Opml(lexeme)
        else if  (lexeme.equals("+") || lexeme.equals("-"))
        	tokens append  new Opad(lexeme)
      }
      case 9 => {	//Separator
    	  	tokens append  new Separator(lexeme)
      }
      case 6 => //Do nothing
    }
  }
  def getNext():LexicCategorie = {
     var token:LexicCategorie  = null;
     
      if(currentToken <= tokens.size-1)
    	   token  =  this.tokens(this.currentToken);
	  currentToken = currentToken + 1;
	 // println(token)
	  token;
  }
  
  private def nextState(c: Char) {
    currentState match {
      case 0 => {
        if (c.toString().matches("[0-9]")){ currentState = 1; return} //Integer
        if (c.toString().matches("\\.")){ currentState = 8; return} // Real Part
        if (c.toString().matches("[\\(\\{\\[\\]\\}\\)]")){ currentState = 3; return} // Group
        if (c.toString().matches("[a-zA-Z]")){ currentState = 4; return} // Identifier
        if (c.toString().matches("[\\+\\-\\*\\/\\^]")){ currentState = 5; return} // Operator
        if (c.toString().matches("\\s")){ currentState = 6; return} // White space
        if (c == ','){currentState = 9;  return;} // Separator
        if (c == '\0'){ currentState = 7; return} // White space
      }
      case 1 => {
        if (c.toString().matches("[0-9]")){ currentState = 1; return} //Integer
        if (c.toString().matches("\\.")){ currentState = 2; return} //Real Part
      }
      case 2 => {
        if (c.toString().matches("[0-9]")){ currentState = 2; return} //Integer
      }
      case 3 => {
        //Do nothing
      }
      case 4 => {
        if (c.toString().matches("[a-zA-Z0-9]")){ currentState = 4; return} // Identifier
      }
      case 5 => {
        //Do nothing
      }
      case 6 => {
        //Do nothing
      }
      case 7 => {
        //Do nothing
      }
      case 8 => {
        if (c.toString().matches("[0-9]")){ currentState = 2; return} //Integer
      }
      case 9=> {
        // Do nothing
      }
    }
    throw new LexicalException
  }
  
 
  
}

