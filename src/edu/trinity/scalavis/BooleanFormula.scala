package edu.trinity.scalavis

import ScalaVis._
import scala.swing._
import scala.swing.event._
import scala.util.parsing.combinator._

class BooleanFormula(val str:String) {
    val tree=DoubleFormula.parseAll(DoubleFormula.BooleanExpression,str).get
    def apply(i:Int,s:Int,x:IOList,y:IOList,vars:Map[String,Double]):Boolean = tree.eval(i,s,x,y,vars)
}

object BooleanFormula {
    def safeRange(s:Int,x:IOList,y:IOList,df:BooleanFormula*):Range = {
        // TODO
        1 to 100
    }

    def editor(init:String,desc:String):Editor = new Editor(init,desc)
    
    class Editor(init:String,desc:String) {
        private var form=new BooleanFormula(init)
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
                            case e:EditDone => form=new BooleanFormula(text)
                        }
                    }-> BorderPanel.Position.Center)
                }
            }
            propPanel
        }
    }

}
