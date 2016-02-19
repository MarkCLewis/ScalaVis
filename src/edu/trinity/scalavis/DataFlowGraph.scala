package edu.trinity.scalavis

import scala.collection.mutable.ArrayBuffer

/**
 * A graph for data flow in ScalaVis
 * 
 * @author Mark Lewis
 */
class DataFlowGraph {
    private val builders=ArrayBuffer[GraphBuilder]()
    private val nodes=ArrayBuffer[ValueNode]()
    
    class GraphBuilder(val builder:Builder) {
        var x=0
        var y=0
        //var label:Label
    }
    
}