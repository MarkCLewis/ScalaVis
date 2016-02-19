package edu.trinity.scalavis

import ScalaVis._
import scala.swing._
import scala.swing.event._
import scala.util.parsing.combinator._

class DoubleFormula(val str:String) {
    private val tree=DoubleFormula.parseAll(DoubleFormula.Expression,str).get
    def apply(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = tree.eval(i,s,x,y,vars)
}

object DoubleFormula extends JavaTokenParsers {
    private val allRange = -1000000000 to 1000000000
    
    def apply(str:String) = new DoubleFormula(str)
    def main(args:Array[String]) {
        println(new DoubleFormula("4+5")(0,0,null,null,null))
        println(new DoubleFormula("4+5*3")(0,0,null,null,null))
        println(new DoubleFormula("(4+5)*3")(0,0,null,null,null))
        println(new DoubleFormula("4+5-3")(0,0,null,null,null))
        println(new DoubleFormula("4*5/3")(0,0,null,null,null))
        println(new DoubleFormula("4+5*i")(0,0,null,null,Map(("i"->3.0))))
        println(new DoubleFormula("y[1]+5*x[0]")(0,0,IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),null))
        println(new DoubleFormula("y[0][1]+5*x[0][0]")(0,0,IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),null))
        println(new DoubleFormula("y[0][0][1]+5*x[0][0][0]")(0,0,IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),IndexedSeq(IndexedSeq(IndexedSeq(Vector(3.0,4.0)))),null))
        println(new DoubleFormula("if(5>3) 4+5*3 else 4+5")(0,0,null,null,null))
    }
    
    def safeRange(s:Int,x:IOList,y:IOList,df:DoubleFormula*):Range = {
//        println("allRange = "+allRange)
        val ret=df.foldLeft(allRange)((r,dblf) => {
            val nr=dblf.tree.safeRange(s,x,y)
//            println(r+" :: "+nr)
            (r.head max nr.head) to (r.last min nr.last)
        } )
        if(ret==allRange) 0 until x.map(_(s).length).max 
        else ret
    }

    def editor(init:String,desc:String):Editor = new Editor(init,desc)
    
    class Editor(init:String,desc:String) {
        private var form=new DoubleFormula(init)
        private var propPanel:Component=null
        def formula = form
        def component = {
            if(propPanel==null) {
                propPanel=new BorderPanel {
                    layout+=(new Label(desc) -> BorderPanel.Position.West)
                    layout+=(new TextField {
                        text=form.str
                        listenTo(this)
                        reactions += {
                            case e:EditDone => form=new DoubleFormula(text)
                        }
                    }-> BorderPanel.Position.Center)
                }
            }
            propPanel
        }
    }
 
    private def Expression:Parser[DoubleNode] = IfExpression | 
        AdditiveExpression
    private def IfExpression:Parser[DoubleNode] = 
        "if(" ~ BooleanExpression ~ ")" ~ Expression ~ "else" ~ Expression ^^ { case _ ~ cond ~ _ ~ texp ~ _ ~ fexp => new IfNode(cond,texp,fexp) }
    private[scalavis] def BooleanExpression:Parser[BooleanNode] = 
        AndExpression ~ rep(OrOperator ~ AndExpression)  ^^ { case (start)~(rest) => if(rest.isEmpty) start else new BooleanTreeNode(start, rest) }
    private def AndExpression:Parser[BooleanNode] = 
        RelationalExpression ~ rep(AndOperator ~ RelationalExpression) ^^ { case (start)~(rest) => if(rest.isEmpty) start else new BooleanTreeNode(start, rest) }
    private def RelationalExpression:Parser[BooleanNode] = 
        AdditiveExpression ~ RelationalOperator ~ AdditiveExpression  ^^ { case (start)~(op)~(rest) => new RelationalTreeNode(start,op,rest) } |
        BooleanPrimaryExpression
    private def AdditiveExpression:Parser[DoubleNode] = 
        MultiplicativeExpression ~ rep(AdditiveOperator ~ MultiplicativeExpression)  ^^ { case (start)~(rest) => if(rest.isEmpty) start else new TreeNode(start, rest) }
    private def MultiplicativeExpression:Parser[DoubleNode] = 
        UnaryExpression ~ rep(MultiplicativeOperator ~ UnaryExpression) ^^ { case (start)~(rest) => if(rest.isEmpty) start else new TreeNode(start, rest) }
    private def UnaryExpression:Parser[DoubleNode] = PrimaryExpression | 
        UnaryOperator ~ UnaryExpression ^^ { case op ~ e => new UnaryNode(op,e) } 
    private def Constructor:Parser[String] = ident ~ repsep(".",ident) ^^ { case s1 ~ s2 => s1+s2.mkString }
    private def PrimaryExpression:Parser[DoubleNode] = "(" ~ Expression ~ ")" ^^ { case _ ~ e ~ _ => e}| 
        floatingPointNumber ^^ (s => new LiteralNode(s.toDouble) ) |
        "x[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ offset ~ _ ~ index ~ _=> new XNode(input.toInt,offset.toInt,index.toInt) } |
        "x[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ index ~ _=> new XNode(input.toInt,0,index.toInt) } |
        "x[" ~ wholeNumber ~ "]" ^^ { case _ ~ index ~ _ => new XNode(0,0,index.toInt) } |
        "y[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ offset ~ _ ~ index ~ _=> new YNode(input.toInt,offset.toInt,index.toInt) } |
        "y[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ index ~ _=> new YNode(input.toInt,0,index.toInt) } |
        "y[" ~ wholeNumber ~ "]" ^^ { case _ ~ index ~ _ => new YNode(0,0,index.toInt) } |
        "cy[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ offset ~ _ ~ index ~ _=> new CYNode(input.toInt,offset.toInt,index.toInt) } |
        "cy[" ~ wholeNumber ~ "][" ~ wholeNumber ~ "]" ^^ { case _ ~ input ~ _ ~ index ~ _=> new CYNode(input.toInt,0,index.toInt) } |
        "cy[" ~ wholeNumber ~ "]" ^^ { case _ ~ index ~ _ => new CYNode(0,0,index.toInt) } |
        ident ^^ (s => new VarNode(s))
    private def BooleanPrimaryExpression:Parser[BooleanNode] = "(" ~ BooleanExpression ~ ")" ^^ { case _ ~ e ~ _ => e} | 
        "false" ^^ (_ => new BooleanLiteralNode(false)) | 
        "true" ^^ (_ => new BooleanLiteralNode(true)) 
    
    private def RelationalOperator:Parser[(Double,Double)=>Boolean] = "==" ^^ (_ => (a:Any,b:Any) => a==b ) | 
        "!=" ^^ (_ => (a:Double,b:Double) => a!=b ) | 
        "<" ^^ (_ => (a:Double,b:Double) => a<b) | 
        ">" ^^ (_ => (a:Double,b:Double) => a>b) | 
        "<=" ^^ (_ => (a:Double,b:Double) => a<=b) |
        ">=" ^^ (_ => (a:Double,b:Double) => a>=b)            
    private def MultiplicativeOperator:Parser[(Double,Double)=>Double] = 
        "*" ^^ (_ => (a:Double,b:Double) => a*b ) | 
        "/" ^^ (_ => (a:Double,b:Double) => a/b ) | 
        "%" ^^ (_ => (a:Double,b:Double) => a%b )
    private def AdditiveOperator:Parser[(Double,Double)=>Double] = 
        "+" ^^ (_ => (a:Double,b:Double) => a+b ) | 
        "-" ^^ (_ => (a:Double,b:Double) => a-b )
    private def UnaryOperator:Parser[(Double)=>Double] = 
        "-" ^^ (_ => (a:Double) => -a ) | 
        "+" ^^ (_ => (a:Double) => a ) 
    private def BooleanUnaryOperator:Parser[(Boolean)=>Boolean] = 
        "!" ^^ (_ => (a:Boolean) => !a)
    private def AndOperator:Parser[(Boolean,Boolean)=>Boolean] = 
        "&&" ^^ (_ => (a:Boolean,b:Boolean) => a && b ) 
    private def OrOperator:Parser[(Boolean,Boolean)=>Boolean] = 
        "||" ^^ (_ => (a:Boolean,b:Boolean) => a || b) 

    private trait DoubleNode { 
        def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double
        def safeRange(s:Int,x:IOList,y:IOList):Range
    }
    private class LiteralNode(v:Double) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = v
        def safeRange(s:Int,x:IOList,y:IOList):Range = allRange
        override def toString:String = "Lit="+v
    }
    private class VarNode(val name:String) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = vars(name)
        def safeRange(s:Int,x:IOList,y:IOList):Range = allRange
    }
    private class XNode(input:Int,offset:Int,index:Int) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = x(input)(if(s<x(input).length) s else 0)(i+offset)(index)
        def safeRange(s:Int,x:IOList,y:IOList):Range = -offset until x(input)(if(s<x(input).length) s else 0).length-offset
    }
    private class YNode(input:Int,offset:Int,index:Int) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = y(input)(if(s<x(input).length) s else 0)(i+offset)(index)
        def safeRange(s:Int,x:IOList,y:IOList):Range = -offset until y(input)(if(s<y(input).length) s else 0).length-offset
    }
    private class CYNode(input:Int,offset:Int,index:Int) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = y(input)(if(s<x(input).length) s else 0)(offset)(index)
        def safeRange(s:Int,x:IOList,y:IOList):Range = allRange
    }
    private class UnaryNode(op:(Double)=>Double,node:DoubleNode) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = op(node.eval(i,s,x,y,vars))
        def safeRange(s:Int,x:IOList,y:IOList):Range = node.safeRange(s,x,y)
    }
    private class TreeNode(start: DoubleNode, ops: List[~[(Double,Double) => Double,DoubleNode]]) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars: Map[String, Double]): Double = (ops foldLeft start.eval(i,s,x,y,vars)){(res, e) => e._1(res, e._2.eval(i,s,x,y,vars))}
        def safeRange(s:Int,x:IOList,y:IOList):Range = ops.foldLeft(start.safeRange(s,x,y))((r,t) => {
            val nr=t._2.safeRange(s,x,y)
            (r.head max nr.head) to (r.last min nr.last)
        } )
        override def toString:String = "TreeNode "+start+" "+ops
    }
    private class IfNode(cond:BooleanNode,texp:DoubleNode,fexp:DoubleNode) extends DoubleNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Double = if(cond.eval(i,s,x,y,vars)) texp.eval(i,s,x,y,vars) else fexp.eval(i,s,x,y,vars)
        def safeRange(s:Int,x:IOList,y:IOList):Range = {
            val cr = cond.safeRange(s,x,y)
            val tr = texp.safeRange(s,x,y)
            val fr = fexp.safeRange(s,x,y)
            (cr.head max tr.head max fr.head) to (cr.last min tr.last min fr.last)
        }
    }
    
    private[scalavis] trait BooleanNode { 
        def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Boolean
        def safeRange(s:Int,x:IOList,y:IOList):Range
    }
    private class BooleanLiteralNode(v:Boolean) extends BooleanNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Boolean = v
        def safeRange(s:Int,x:IOList,y:IOList):Range = allRange
        override def toString:String = "Lit="+v
    }
    private class BooleanUnaryNode(op:(Boolean)=>Boolean,node:BooleanNode) extends BooleanNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Boolean = op(node.eval(i,s,x,y,vars))
        def safeRange(s:Int,x:IOList,y:IOList):Range = node.safeRange(s,x,y)
    }
    private class BooleanTreeNode(start: BooleanNode, ops: List[~[(Boolean,Boolean) => Boolean,BooleanNode]]) extends BooleanNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars: Map[String,Double]): Boolean = (ops foldLeft start.eval(i,s,x,y,vars)){(res, e) => e._1(res, e._2.eval(i,s,x,y,vars))}
        def safeRange(s:Int,x:IOList,y:IOList):Range = ops.foldLeft(start.safeRange(s,x,y))((r,t) => {
            val nr=t._2.safeRange(s,x,y)
            (r.head max nr.head) to (r.last min nr.last)
        } )
        override def toString:String = "TreeNode "+start+" "+ops
    }

    private class RelationalTreeNode(start: DoubleNode, op: (Double,Double) => Boolean, rest:DoubleNode) extends BooleanNode {
        override def eval(i:Int,s:Int,x:IOList,y:IOList,vars: Map[String,Double]): Boolean = op(start.eval(i,s,x,y,vars),rest.eval(i,s,x,y,vars))
        def safeRange(s:Int,x:IOList,y:IOList):Range = {
            val sr = start.safeRange(s,x,y)
            val rr = rest.safeRange(s,x,y)
            (sr.head max rr.head) to (sr.last min rr.last)
        }
    }
}