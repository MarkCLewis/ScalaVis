package edu.trinity.scalavis.plot

class LinearTransform(vMin:Double,vMax:Double) extends AxisTransform {
    def apply(rangeMin:Double,rangeMax:Double):(Double=>Double) = v => 
        rangeMin+(v-vMin)/(vMax-vMin)*(rangeMax-rangeMin) 
}