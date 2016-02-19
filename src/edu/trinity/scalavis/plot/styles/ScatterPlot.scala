package edu.trinity.scalavis.plot.styles

import edu.trinity.scalavis.ScalaVis._
import edu.trinity.scalavis._
import edu.trinity.scalavis.plot._
import java.awt.Graphics2D
import java.awt.geom._

class ScatterPlot(xForm:DoubleFormula,yForm:DoubleFormula) extends BoundRenderable {
    def render(g:Graphics2D,bounds:Rectangle2D,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double),otherInfo:Any*):List[Plot.Controller] = {
        val numStreams = x.map(_.length).max
        println(bounds)
        for(s <- 0 until numStreams) {
            val safeRange = DoubleFormula.safeRange(s,x,y,xForm,yForm)
            for(i <- safeRange) {
                val (cx,cy)=trans(xForm(i,s,x,y,vars),yForm(i,s,x,y,vars))
                g.fill(new Ellipse2D.Double(cx-1,cy-1,3,3))
            }
        }
        Nil
    }
}