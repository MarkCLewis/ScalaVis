package edu.trinity.scalavis.elements

import edu.trinity.scalavis._ 
import ScalaVis._
import editable.BooleanEditable
import scala.swing._
import javax.swing.BorderFactory

class SelectionFilter(val choice:BooleanFormula,subset:Boolean,subsetExpression:DoubleFormula) extends Element {
    def apply(x:IOList,y:IOList):IOList = {
        if(!subset) {
            IndexedSeq(for(s <- 0 until x(0).length) yield {
                for(i <- BooleanFormula.safeRange(s,x(0)(s),y,choice); if(choice(i,s,x,y,null))) yield x(0)(s)(i)
            })
        } else {
            val takeSet=Set() ++ (for(s <- 0 until x(0).length; i <- BooleanFormula.safeRange(s,x(0)(s),y,choice); if(choice(i,s,x,y,null))) yield subsetExpression(i,s,x,y,null))
            IndexedSeq(for(s <- 0 until x(0).length) yield {
                for(i <- BooleanFormula.safeRange(s,x(0)(s),y,choice); if(takeSet.contains(subsetExpression(i,s,x,y,null)))) yield x(0)(s)(i)
            })
        }
    }
}

object SelectionFilter {
    def apply(choice:BooleanFormula,subset:Boolean=false,subsetExpression:DoubleFormula=new DoubleFormula("x[0]")) = new SelectionFilter(choice,subset,subsetExpression)
    def builder(init:String="true",initSubset:Boolean=true,initSubsetExpr:String="x[0]") = new Builder {
        val choiceEd=BooleanFormula.editor(init,"Selection Formula")
        val subsetEd=new BooleanEditable(initSubset,"Use Subsets?")
        val subsetExpressionEd=DoubleFormula.editor(initSubsetExpr,"Subset Match Expression")
        def buildElement():Element = new SelectionFilter(choiceEd.formula,subsetEd.value,subsetExpressionEd.formula)
        def numXInputs:Int = 1
        def numYInputs:Int = 1
        def numOutputs:Int = 1
        def component = new BoxPanel(Orientation.Vertical) {
            contents+=choiceEd.component
            contents+=new GridPanel(2,1) {
                contents+=subsetEd.component
                contents+=subsetExpressionEd.component
                border=BorderFactory.createTitledBorder("Subsets")
            }
        }
    }
}