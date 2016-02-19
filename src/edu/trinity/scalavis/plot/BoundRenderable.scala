package edu.trinity.scalavis.plot

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import edu.trinity.scalavis.ScalaVis._

trait BoundRenderable {
    def render(g:Graphics2D,bounds:Rectangle2D,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double),otherInfo:Any*):List[Plot.Controller]
}