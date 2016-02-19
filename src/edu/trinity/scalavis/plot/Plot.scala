package edu.trinity.scalavis.plot

import edu.trinity.scalavis.ScalaVis._
import edu.trinity.scalavis._
import java.awt.{Graphics2D,Color,RenderingHints}
import java.awt.geom._
import java.awt.image.BufferedImage
import scala.collection.mutable
import scala.swing._

final class Plot(initContents:BoundRenderable*) extends Element {
    private val contents = mutable.Buffer(initContents:_*)
    private var lastX:IOList = null
    private var lastY:IOList = null
    private var vars = Map[String,Double]()
    private var views = List[Plot.View]()
    
    def apply(x:IOList,y:IOList):IOList = {
        lastX = x
        lastY = y
        views.foreach(_.modelUpdate(this))
        IndexedSeq()
    }

    def +=(p:BoundRenderable):Plot = {
        contents+=p
        this
    }
    
    def addView(newViews:Plot.View*) {
        newViews.foreach(views ::= _)
    }
    
    def removeView(view:Plot.View) {
        views=views.filterNot(v => v==view)
    }
    
    def render(g:Graphics2D,bounds:Rectangle2D):List[Plot.Controller] = {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
        g.setPaint(Color.white)
        g.fill(bounds)
        g.setPaint(Color.black)
        (PlotController :: contents.toList.flatMap(_.render(g,bounds,lastX,lastY,vars,(x,y) => (x,y)))).reverse
    }
    
    object PlotController extends Plot.Controller {
        def active(px:Int,py:Int):Boolean = true
        
    }
}

object Plot {
    trait View {
        def modelUpdate(p:Plot)
    }
    trait Controller {
        def active(px:Int,py:Int):Boolean
    }
    
    class ImageView(img:BufferedImage) extends View {
        def modelUpdate(p:Plot) {
            p.render(img.createGraphics,new Rectangle2D.Double(0,0,img.getWidth-1,img.getHeight-1))
        }
    }
    
    class FrameView extends View {
        var img = new BufferedImage(500,500,BufferedImage.TYPE_INT_ARGB)
        val panel = new Panel {
            override def paint(g:Graphics2D) {
                g.drawImage(img,0,0,null)
            }
            preferredSize = new Dimension(img.getWidth,img.getHeight)
        }
        val frame = new Frame {
            title = "Plot Frame"
            contents = panel
        }
        def modelUpdate(p:Plot) {
            if(panel.size.width>img.getWidth || panel.size.height>img.getHeight) {
                img = new BufferedImage(panel.size.width,panel.size.height,BufferedImage.TYPE_INT_ARGB)
            }
            p.render(img.createGraphics,new Rectangle2D.Double(0,0,panel.size.width-1,panel.size.height-1))
            panel.repaint()
        }
    }
    
    def main(args:Array[String]) {
        val data=IndexedSeq.tabulate(20)(i => Vector(i/20.0))
        val plot=new Plot(Plot2D())
        val frameView=new FrameView
        plot.addView(frameView)
        frameView.frame.visible=true
        plot.apply(data,IndexedSeq())
    }
}
