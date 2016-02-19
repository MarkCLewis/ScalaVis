package edu.trinity.scalavis.editable

import scala.swing._
import scala.swing.event._

class StringEditable(init:String,desc:String) {
    private var value_ = init
    private var propPanel:Component=null
    
    def value = value_
    
    def component = {
        if(propPanel==null) {
            propPanel=new BorderPanel {
                layout+=(new Label(desc) -> BorderPanel.Position.West)
                layout+=(new TextField(init) {
                    listenTo(this)
                    reactions += {
                        case e:EditDone => value_ = text
                    }
                } -> BorderPanel.Position.Center)
            }
        }
        propPanel
    }
}