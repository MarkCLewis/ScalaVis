package edu.trinity.scalavis.plot

import edu.trinity.scalavis.ScalaVis._
import edu.trinity.scalavis._
import java.awt.Graphics2D
import java.awt.geom._

case class Axis(label:Option[ScaledRenderable],
        tickSequence:IndexedSeq[(Option[ScaledRenderable],Double)],
        transform:AxisTransform,
        minSide:Boolean) extends BoundRenderable {
    
    def render(g:Graphics2D,bounds:Rectangle2D,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double),otherInfo:Any*):List[Plot.Controller] = {
        val horizontal=if(otherInfo.isEmpty) true else if(otherInfo(0).isInstanceOf[Boolean])
            otherInfo(0).asInstanceOf[Boolean] else true
        val tickBounds:Rectangle2D = new Rectangle2D.Double(0,0,0,0)
        for((Some(rend),_) <- tickSequence) {
            tickBounds.add(rend.defaultSize(g))
        }
        val (tickFrac,tickLabelFrac,labelFrac,tickScale) = if(tickBounds.getWidth==0) {
                (0.1,0.0,0.9,1.0)
            } else {
                if(label==None)
                    (0.1,0.9,0.0,(if(horizontal) bounds.getHeight else bounds.getWidth)*0.9/tickBounds.getWidth)
                else
                    (0.1,0.6,0.3,(if(horizontal) bounds.getHeight else bounds.getWidth)*0.6/tickBounds.getWidth)
            }
        val labelSize = label match {
            case None => 0
            case Some(rend) =>
                val labelBounds=rend.defaultSize(g)
                if(horizontal) {
                    val scale=bounds.getHeight*labelFrac/labelBounds.getHeight
                    val cx=bounds.getCenterX
                    val cy=bounds.getMinY+(tickFrac+tickLabelFrac)*bounds.getHeight*(if(minSide) 1 else -1)
                    rend.render(g,cx-labelBounds.getWidth*0.5*scale,bounds.getMaxY-labelBounds.getMaxY*scale,
                            scale,x,y,vars,trans)
                } else {
                    val scale=bounds.getWidth*labelFrac/labelBounds.getHeight
                    val cx=bounds.getMinX+(0.5*labelFrac)*bounds.getWidth*(if(minSide) 1 else -1)
                    val cy=bounds.getCenterY
                    val oldTrans=g.getTransform
                    g.rotate(math.Pi*0.5,cx,cy)
                    rend.render(g,bounds.getMinX+labelBounds.getMaxY*scale,cy-labelBounds.getHeight*0.5*scale,
                            scale,x,y,vars,trans)
                    g.setTransform(oldTrans)
                }
                labelBounds.getHeight
        }
        val axTrans=transform(bounds.getMinX,bounds.getMaxX)
        val ayTrans=transform(bounds.getMaxY,bounds.getMinY)
        val aTrans=if(horizontal) (x:Double,y:Double) => (axTrans(x),0.0)
            else (x:Double,y:Double) => (0.0,ayTrans(y))
        val b = if(horizontal)
                if(minSide) bounds.getMinY else bounds.getMaxY
            else if(minSide) bounds.getMaxX else bounds.getMinX 
        val l = if(horizontal) bounds.getHeight*tickFrac else bounds.getWidth*tickFrac
        for(tick <- tickSequence) {
            tick match {
                case (None,v) =>
                    drawTick(v,b,l,g,aTrans,horizontal)
                case (Some(rend),v) =>
                    drawTick(v,b,l,g,aTrans,horizontal)
                    val labelBounds=rend.defaultSize(g)
                    if(horizontal) {
                        val scale=bounds.getHeight*labelFrac/labelBounds.getHeight
                        val cx=bounds.getCenterX
                        val cy=bounds.getMinY+(tickFrac+tickLabelFrac)*bounds.getHeight*(if(minSide) 1 else -1)
                        rend.render(g,cx-labelBounds.getWidth*0.5*scale,bounds.getMaxY-labelBounds.getMaxY*scale,
                                scale,x,y,vars,trans)
                    } else {
                        val scale=bounds.getWidth*labelFrac/labelBounds.getHeight
                        val cx=bounds.getMinX+(0.5*labelFrac)*bounds.getWidth*(if(minSide) 1 else -1)
                        val cy=bounds.getCenterY
                        val oldTrans=g.getTransform
                        g.rotate(math.Pi*0.5,cx,cy)
                        rend.render(g,bounds.getMinX+labelBounds.getMaxY*scale,cy-labelBounds.getHeight*0.5*scale,
                                scale,x,y,vars,trans)
                        g.setTransform(oldTrans)
                    }
            }
        }
        Nil
    }
    
    private def drawTick(v:Double,b:Double,l:Double,g:Graphics2D,trans:(Double,Double)=>(Double,Double),horizontal:Boolean) {
        if(horizontal) {
            val (x,_)=trans(v,0)
            g.draw(new Line2D.Double(x,b-l,x,b+l))
        } else {
            val (_,y)=trans(0,v)
            g.draw(new Line2D.Double(b-l,y,b+l,y))
        }
    }
}