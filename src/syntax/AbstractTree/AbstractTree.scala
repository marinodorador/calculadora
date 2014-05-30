package syntax.AbstractTree
import scala.collection.mutable.ListBuffer
//x^2 
abstract class AbstractTree;

case class Function(i:AbstractTree,p:ListBuffer[AbstractTree]) extends AbstractTree{
  var id:AbstractTree = i
  var params: ListBuffer[AbstractTree] = p
  override def toString():String = {
     this.id.asInstanceOf[Id] + "("+params.mkString(",")+")"
  }
}

abstract class Binary  extends AbstractTree{
  var l: AbstractTree
  var r: AbstractTree 
}

case class Sum(a:AbstractTree, b:AbstractTree) extends Binary{
  var l = a
  var r = b
  override def toString():String = {
    "("+this.l+"+"+this.r+")"
  }
}
case class Rest(a:AbstractTree, b:AbstractTree) extends Binary{
  var l = a
  var r = b
  override def toString():String = {
    "("+this.l+"-"+this.r+")"
  }
}

case class Prod(a:AbstractTree, b:AbstractTree) extends Binary{
  var l = a
  var r = b
  override def toString():String = {
    "("+this.l+"*"+this.r+")"
  }
}
case class Div(a:AbstractTree, b:AbstractTree) extends Binary{
  var l = a
  var r = b
  override def toString():String = {
    "("+this.l+"/"+this.r+")"
  }
}
case class Power(a:AbstractTree, b:AbstractTree) extends Binary{
  var l = a
  var r = b
  override def toString():String = {
    if(this.r.isInstanceOf[Binary])
    	l+"^("+this.r+")"
    else
    	this.l+"^"+this.r
  }
}
case class Vector(params: ListBuffer[AbstractTree]) extends AbstractTree{
  override def toString():String = {
    "["+params.mkString(",")+"]"
  }
}
case class Sign(o: AbstractTree) extends AbstractTree{
  override def toString():String = {
    "-"+o
  }
}
case class Const(v:Double) extends AbstractTree{
  override def toString():String = {
    ""+v
  }
}
case class Id(id:String) extends AbstractTree{
  override def toString():String = {
   id
  }
}
