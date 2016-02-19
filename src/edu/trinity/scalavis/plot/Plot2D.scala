package edu.trinity.scalavis.plot

import edu.trinity.scalavis.ScalaVis._
import edu.trinity.scalavis._
import java.awt.{Graphics2D,Font,Color}
import java.awt.geom._
import scala.swing.Component
import scala.collection.mutable

/**
 * This class defines a 2-D plot area. It can be a grid with multiple axes and multiple data sets.
 */
final class Plot2D(initialSetup:Seq[Seq[Plot2D.Cell]],rh:Seq[Double],cw:Seq[Double]) extends BoundRenderable {
    private val setup = if(initialSetup.isEmpty) mutable.Buffer[mutable.Buffer[Plot2D.Cell]]() 
        else mutable.Buffer.tabulate(initialSetup.length,initialSetup(0).length)((i,j) => initialSetup(i)(j))
    private val rowHeights = mutable.Buffer(rh:_*)
    private val columnWidths = mutable.Buffer(cw:_*)
    private val wholeFrac = new FracRegion(0,0,1,1)
    private var Seq(xminAxes,yminAxes,xmaxAxes,ymaxAxes)=compileAxes
    private val gridFrac = {
        val xmin=xminAxes.map(_.length).max
        val xmax=xmaxAxes.map(_.length).max
        val ymin=yminAxes.map(_.length).max
        val ymax=ymaxAxes.map(_.length).max
        new FracRegion(0.15*ymin,0.15*xmax,1.0-0.15*(ymin+ymax),1.0-0.15*(xmin+xmax))
    }

    def render(g:Graphics2D,bounds:Rectangle2D,x:IOList,y:IOList,vars:Map[String,Double],
            trans:(Double,Double)=>(Double,Double),otherInfo:Any*):List[Plot.Controller] = {
        val wholeBounds = wholeFrac ** bounds
        val gridBounds = gridFrac ** bounds
        val rhSum = rowHeights.sum
        val cwSum = columnWidths.sum
        var rhAccum = 0.0
        var ret=List[Plot.Controller]()
        // Render data sets
        for(r <- setup.indices) {
            var cwAccum = 0.0
            for(c <- setup(r).indices) {
                val cellBounds = (new FracRegion(cwAccum/cwSum,rhAccum/rhSum,columnWidths(c)/cwSum,rowHeights(r)/rhSum)) ** gridBounds
                for(daa <- setup(r)(c).dataAndAxes) {
                    val axTrans=daa.xAxis.transform(cellBounds.getMinX,cellBounds.getMaxX)
                    val ayTrans=daa.yAxis.transform(cellBounds.getMaxY,cellBounds.getMinY)
                    def aTrans(px:Double,py:Double):(Double,Double) = {
                        (axTrans(px),ayTrans(py))
                    }
                    val clip = g.getClip
                    g.setClip(cellBounds)
                    ret :::= daa.dataSet.render(g,cellBounds,x,y,vars,aTrans)
                    g.setClip(clip)
                    g.draw(cellBounds)
                }
                cwAccum += columnWidths(c)
            }
            rhAccum += rowHeights(r)
        }
        // Render axes
        {
            val numAxes = xminAxes.map(_.length).max
            val fullAxisBounds=new Rectangle2D.Double(gridBounds.getMinX,gridBounds.getMaxY,gridBounds.getWidth,wholeBounds.getMaxY-gridBounds.getMaxY)
            val xStep=fullAxisBounds.getWidth/xminAxes.length
            val yStep=fullAxisBounds.getHeight/numAxes
            for(i <- xminAxes.indices) {
                for(j <- xminAxes(i).indices) {
                    val axisBounds=new Rectangle2D.Double(fullAxisBounds.getMinX+i*xStep,
                        fullAxisBounds.getMinY+j*yStep,xStep,yStep)
                    xminAxes(i)(j).render(g,axisBounds,x,y,vars,trans,true)
                }
            }
        }
        {
            val numAxes = yminAxes.map(_.length).max
            val fullAxisBounds=new Rectangle2D.Double(wholeBounds.getMinX,wholeBounds.getMinY,gridBounds.getMinX-wholeBounds.getMinX,gridBounds.getHeight)
            val xStep=fullAxisBounds.getWidth/numAxes
            val yStep=fullAxisBounds.getHeight/yminAxes.length
            for(i <- yminAxes.indices) {
                for(j <- yminAxes(i).indices) {
                    val axisBounds=new Rectangle2D.Double(fullAxisBounds.getMaxX-(j+1)*xStep,
                        fullAxisBounds.getMinY+i*yStep,xStep,yStep)
                    yminAxes(i)(j).render(g,axisBounds,x,y,vars,trans,false)
                }
            }
        }
        ret
    }
    
    /**
     * This method compiles the axis information and returns a list of axes for each side and
     * cell.  The top level sequence is x_min, y_min, x_max, y_max. The next level is position
     * along the cells for that side. The last one is all the axes on that side. 
     */
    private def compileAxes:Seq[Seq[Seq[Axis]]] = {
        if(setup.isEmpty) Seq()
        else {
            val (xmin,xmax)=setup.map(r => r.flatMap(_.dataAndAxes.map(_.xAxis)).partition(_.minSide)).unzip
            val (ymin,ymax)=setup.transpose.map(c => c.flatMap(_.dataAndAxes.map(_.yAxis)).partition(_.minSide)).unzip
            println(xmin+" "+ymin+" "+xmax+" "+ymax)
            Seq(xmin,ymin,xmax,ymax)
        }
        // TODO: optimize order so more things can be joined.
    }
}

object Plot2D {
    case class DataAndAxes(dataSet:BoundRenderable,xAxis:Axis,yAxis:Axis)
    
    class Cell(val dataAndAxes:mutable.Buffer[DataAndAxes]) {
        // legend
        // listeners
        // labels
    }
    
    def scatterCell:Cell = {
        val sp=new styles.ScatterPlot(DoubleFormula("x[0]"),DoubleFormula("x[0]*x[0]"))
        val xAxis=new Axis(Some(RenderString("x Axis")),
            (0.0 to 1.0 by 0.2).map(v => (Some(RenderString(v)),v)),new LinearTransform(0.0,1.0),true)
        val yAxis=new Axis(Some(RenderString("y Axis")),
            (0.0 to 1.0 by 0.2).map(v => (Some(RenderString(v)),v)),new LinearTransform(0.0,1.0),true)
        new Plot2D.Cell(mutable.Buffer(Plot2D.DataAndAxes(sp,xAxis,yAxis)))
    }
    
    def apply():Plot2D = new Plot2D(Seq(Seq(scatterCell)),Seq(1.0),Seq(1.0))
}