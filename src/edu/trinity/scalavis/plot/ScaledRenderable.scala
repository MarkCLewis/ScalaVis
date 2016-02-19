package edu.trinity.scalavis.plot

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import edu.trinity.scalavis.ScalaVis._

trait ScaledRenderable {
    def render(g:Graphics2D,px:Double,py:Double,scale:Double,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double)):List[Plot.Controller]
    
    def defaultSize(g:Graphics2D):Rectangle2D

    def scaledSize(scale:Double,g:Graphics2D):Rectangle2D
}