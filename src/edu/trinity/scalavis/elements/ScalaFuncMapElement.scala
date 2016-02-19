package edu.trinity.scalavis.elements

import edu.trinity.scalavis._ 
import ScalaVis._

class ScalaFuncMapElement(val f:Vector[Double]=>Vector[Double]) extends Element {
    def apply(x:IOList,y:IOList):IOList = {
        Vector(x(0).map(_.map(f)))
    }
}

object ScalaFuncMapElement {
    def apply(f:Vector[Double]=>Vector[Double]) = new ScalaFuncMapElement(f)
}