package edu.trinity.scalavis.elements

import edu.trinity.scalavis._
import ScalaVis._
import edu.trinity.scalavis.editable._
import scala.collection.mutable.ArrayBuffer
import scala.swing._

class FunctionFilter(valFuncs:Seq[DoubleFormula]) extends Element {
    def apply(x:IOList,y:IOList):IOList = {
        IndexedSeq(for(s <- 0 until x(0).length) yield {
            for(i <- DoubleFormula.safeRange(s,x(0)(s),y,valFuncs:_*)) yield {
                Vector(valFuncs.map(f => f(i,s,x,y,null)):_*)
            }
        })
    }
}

object FunctionFilter {
    def apply(valFuncs:DoubleFormula*) = new FunctionFilter(valFuncs)
    def builder(names:Seq[String],forms:Seq[String]) = new Builder {
        class Pair(n:String,f:String) extends Editable {
            val fname=new StringEditable(n,"Name")
            val form=DoubleFormula.editor(f,"Formula")
            def component:Component = new GridPanel(2,1) {
                contents+=fname.component
                contents+=form.component
            }
            override def toString = fname.value+" : "+form.formula.str
        }
        val pairs=ArrayBuffer((names,forms).zipped.map((n,f) => new Pair(n,f)):_*)
        def buildElement():Element = new FunctionFilter(pairs.map(p => p.form.formula))
        def numXInputs:Int = 1
        def numYInputs:Int = 1
        def numOutputs:Int = 1
        def component = new GroupEditable(pairs,new Pair("Default","0")).component
    }
}