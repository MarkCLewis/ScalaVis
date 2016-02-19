package edu.trinity.scalavis.plot

class LogTransform(vMin:Double,vMax:Double) extends AxisTransform {
    private val vlMin=math.log10(vMin)
    private val vlMax=math.log10(vMax)
    def apply(rangeMin:Double,rangeMax:Double):(Double=>Double) = v => rangeMin+(math.log10(v)-vlMin)/(vlMax-vlMin)*(rangeMax-rangeMin) 
}