package edu.trinity.scalavis.plot

import java.awt.geom._

final class FracRegion(var x:Double,var y:Double,var width:Double,var height:Double) {
    def **(r:Rectangle2D):Rectangle2D = {
        new Rectangle2D.Double(r.getX+x*r.getWidth,r.getY+y*r.getHeight,width*r.getWidth,height*r.getHeight)
    }
}