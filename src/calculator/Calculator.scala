package calculator
import lexic._
import syntax.AbstractTree._
import syntax._
import semantic._
import strategies.CalculatorStrategy

class Calculator {
   
   var iStrategy:CalculatorStrategy = null
   var expresion:String = null
   
   def this(exp:String){
     this()
     this.expresion = exp
   }
   
   def calculate():AbstractTree = {
     var alex:LexicAnalizer = new LexicAnalizer(this.expresion)
     alex.analize
     var sintax:SyntacticAnalizer = new SyntacticAnalizer(alex)
     sintax.analize
     var semantic:SemanticAnalizer = new SemanticAnalizer(sintax.getTree)
     semantic.analize
     iStrategy = semantic.getStrategy
     iStrategy.calculate()
   }
}

class Test{
  def test(foo:String){
    //println(foo)
     var s:SyntacticAnalizer = null
     var l:LexicAnalizer = null
     var result:Boolean = false
     l = new LexicAnalizer(foo);
     l.analize
     s = new SyntacticAnalizer(l)
     result = s.analize
    println("--------------------------------------------------------------------------------------------")
    println(foo+" <expected value>: true, <getted value>:"+result+(if (result) " passed" else " failed"));
    println("Tree: "+s.getTree)
  }
  def testCalc(foo:String){
    var c:Calculator = new Calculator(foo);
    val tree:AbstractTree = c.calculate()
    println("Result tree: "+tree)
  }
  def main(args:Array[String]){
    
    /*Test 1 (polinomica)*/
    test("derive({-3x^3+2x^2-2x+23},x,10)")
    testCalc("derive({-3x^3+2x^2-2x+23},x,10)")
    
   /*Test 2 (exponencial)*/
    test("derive({2^x},x,10)")
    testCalc("derive({2^x},x,10)")
    /*Test 3 (trigonometrica)*/
    test("derive({-sen(x)+3cos(x)},x,10)")
    testCalc("derive({-sen(x)+3cos(x)},x,10)")
   // test("derive({2sen(x)3cos(x)},x,10)")
    /*Test 4 (cociente)*/
   test("derive({(2x^2+3x^3)/(2x)},x,10)")
   testCalc("derive({(2x^2+3x^3)/(2x)},x,10)")
    /*Test 4 (producto)*/
   test("derive({(2x^2+3x^3)*(2x)},x,10)")
   testCalc("derive({(2x^2+3x^3)*(2x)},x,10)") 
    /*Test 5 (producto)*/
   test("derive({(2x^2+3x^3)(2x)},x,10)")
   testCalc("derive({(2x^2+3x^3)(2x)},x,10)")
   /*Test 6 (producto)*/
   test("derive({(2x^2*10x*10+3x^3)(2x)},x,10)")
   testCalc("derive({(2x^2*10x*10+3x^3)(2x)},x,10)")
   
  /*Test 7 (producto)*/
   
   test("integra({(3x^3)(2x)},x,[2,5])")
   testCalc("integra({(3x^3)(2x)},x,[2,5])")
   /*Test 8 (expresión)*/
   /*test("3x+3y+32")*/
   
  /*Test 7 (lineal)*/
   test("integra({(2x)},x,[2,10])")
   testCalc("integra({(2x)},x,[2,10])")
  /*Test 3 (trigonometrica)*/
    test("integra({-sen(x)+3cos(x)},x,[2,10])")
    testCalc("integra({-sen(x)+3cos(x)},x,[2,10])")
   /*Test 8 (expresión)*/
    test("3(23)+3*23+32");
    testCalc("3(23)+3*23+32");
  }
}