package edu.trinity.scalavis.editable

import scala.swing._
import scala.swing.event._

class GroupEditable[T <: Editable](init:scala.collection.mutable.Buffer[T],cons: =>T,copy:(T)=>T=null) extends Editable {
    def component:Component = new GridPanel(2,1) {
        val list=new ListView(init)
        contents+=new BorderPanel {
            layout+=(new ScrollPane(list) -> BorderPanel.Position.Center)
            layout+=(new GridPanel(1,3) {
                contents+=new Button(Action("Add") { init+=cons })
                contents+=new Button(Action("Remove") {
                    if(!list.selection.indices.isEmpty) {
                        init.remove(list.selection.indices.head)
                    }
                })
                if(copy!=null) {
                    contents+=new Button(Action("Duplicate") {
                        if(!list.selection.indices.isEmpty) {
                            init+=copy(init(list.selection.indices.head))
                        }
                    })
                }
            } -> BorderPanel.Position.South)
        }
        val opts=new BorderPanel
        contents+=new ScrollPane(opts)
        listenTo(list.selection)
        reactions += {
            case ListSelectionChanged(source,range,live) => {
                opts.layout.clear()
                if(!list.selection.indices.isEmpty) {
                    opts.layout+=(init(list.selection.indices.head).component -> BorderPanel.Position.North)
                }
                opts.revalidate
            }
        }
    }
}