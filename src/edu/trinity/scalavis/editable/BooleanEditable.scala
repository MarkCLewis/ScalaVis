package edu.trinity.scalavis.editable

import scala.swing._
import scala.swing.event._

class BooleanEditable(init:Boolean,desc:String) {
    private var value_ = init
    private var propPanel:Component=null
    
    def value = value_
    
    def component = {
        if(propPanel==null) {
            propPanel=new CheckBox(desc) {
                selected=value
                listenTo(this)
                reactions += {
                    case e:ButtonClicked => value_ = selected
                }
            }
        }
        propPanel
    }
}