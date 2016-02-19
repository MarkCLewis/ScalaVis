package edu.trinity.scalavis.plot

import edu.trinity.scalavis._

class AxisBuilder {
//    def buildX:Axis = Axis(Option(label),
//        (min to max by spacing).map(d => 
//            (Some(new RenderString(d.toString,DoubleFormula(d.toString),DoubleFormula("0"))),d)),
//        new LinearTransform(min,max),drawTicks,drawValues)
    
    var label="Default"
    var min=0.0
    var max=1.0
    var spacing=0.2
    var transformType=0
    var drawTicks=true
    var drawValues=true
}