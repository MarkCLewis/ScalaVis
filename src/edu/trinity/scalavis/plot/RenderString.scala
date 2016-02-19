package edu.trinity.scalavis.plot

import java.awt.{Graphics2D,Font,Paint,Color}
import java.awt.geom._
import edu.trinity.scalavis._
import edu.trinity.scalavis.ScalaVis._

class RenderString(private val str:String,font:Font,paint:Paint,orientation:Double) extends ScaledRenderable {
    def render(g:Graphics2D,px:Double,py:Double,scale:Double,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double)):List[Plot.Controller] = {
        val (dx,dy)=trans(px,py)
        val oldPaint=g.getPaint
        g.setPaint(paint)
        val oldFont=g.getFont
        g.setFont(font.deriveFont(font.getSize*scale.toFloat))
        val oldTrans=g.getTransform
        g.rotate(orientation,dx,dy)
        g.drawString(str,dx.toFloat,dy.toFloat)
        g.setPaint(oldPaint)
        g.setFont(oldFont)
        g.setTransform(oldTrans)
        Nil
    }
    
    def defaultSize(g:Graphics2D):Rectangle2D = {
        val oldTrans=g.getTransform
        g.rotate(orientation)
        val ret=font.getStringBounds(str,g.getFontRenderContext)
        g.setTransform(oldTrans)
        ret
    }
    
    def scaledSize(scale:Double,g:Graphics2D):Rectangle2D = {
        val oldTrans=g.getTransform
        g.rotate(orientation)
        val ret=font.deriveFont(font.getSize*scale.toFloat).getStringBounds(str,g.getFontRenderContext)
        g.setTransform(oldTrans)
        ret
    }
    
    override def equals(other:Any):Boolean = {
        if(other.isInstanceOf[RenderString])
            str == other.asInstanceOf[RenderString].str
        else false
    }
}

object RenderString {
    val defaultFont=new Font(Font.SERIF,Font.PLAIN,12)
    def apply(str:String) = new RenderString(str,defaultFont,Color.black,0.0)
    def apply(v:Double) = new RenderString(v.toString,defaultFont,Color.black,0.0)
}