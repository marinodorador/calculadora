package calculator;

import syntax.AbstractTree.AbstractTree;

public class Runnable {
	public static void main(String args[]){
		
		if(args.length == 0 || args[0].equals("-test")){
			Test t = new Test();
			t.main(args);
		}else{
			System.out.println(args[0]);
			 Calculator c  = new Calculator(args[0]);
			 AbstractTree tree = c.calculate();
			 System.out.println("Resultado: "+tree);
		}
	}
}
