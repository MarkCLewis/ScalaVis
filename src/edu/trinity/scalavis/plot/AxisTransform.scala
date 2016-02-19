package edu.trinity.scalavis.plot

abstract class AxisTransform extends ((Double,Double)=>(Double=>Double)) {
    def apply(rangeMin:Double,rangeMax:Double):(Double=>Double)
}