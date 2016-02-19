package edu.trinity.scalavis

import scala.swing._

/**
 * This is the main entry point for ScalaVis.  If entered in this way, the program will
 * run just like SwiftVis with a few minor differences.  I'm thinking of using Vector[Double]
 * for my DataElement.  The ints for params weren't really needed.  I can fold those into
 * doubles and things will work just as well.  I feel like I should start with plotting and
 * then build backwards from there.
 * 
 * Some of the bigger questions I have relate to how to tie this into the REPL and scripting.
 * The GraphPanel in SwiftVis became a singleton and handled threading/scheduling issues.
 * I later realized that was a bad decision.  I want to be able to have multiple graphs active
 * at the same time.
 * 
 * Have futures for threading to deal with scripts.  Allow normal connections or functional
 * interaction with elements.  The functional interaction implies that data isn't associated
 * with the elements (except maybe sources).  That leads to a fundamentally different data model.
 * Filters take in data and return data.  Allow multiple outputs and parallel streams.  Have
 * nodes that "hold" the data as connections.  One advantage of this is that I can have multiple
 * outputs separate from multiple streams.  A stream represents identical processing on
 * separate data.  Different outputs would represent the different things that something can
 * spit out.  There could also be multiple inputs.  It is worth thinking about the "types"
 * if inputs.  There could be processed inputs that come in from the left and while I can
 * only think of reasons for one tab there, it might be that there could be several like for a
 * correlation filter.  Then there could also be inputs from the top/bottom that provide
 * additional information.  The question is, is that distinction needed or would it add
 * complexity without significant benefit like the parameters.  I think not because of stream
 * handling and indexing.
 * 
 * So the "functions" can have a signature that looks like this: 
 * f(x:IndexedSeq[DataSet],y:IndexedSeq[DataSet]):IndexedSeq[DataSet].
 * I need to think about how to make it easy to come up with these things command line.  In
 * formulas you could have x[input][offset][value] and y[input][stream][offset][value].  The
 * brackets would be optional in the order of offset, stream, input.  You can't specify a stream
 * on x because the difference between x and y is that all x arguments have to be data parallel
 * across streams.  In the graphical interface, if there are things that would lead to errors (like
 * x values with different numbers of streams) then the box should "glow red".
 * 
 * Each elements is a function.  One could think of settings as state, but it could also be
 * a first, curried argument.  So elem(state)(x,y).  In this sense, the GUI is nothing more than
 * a function builder that has state.  For scripting purposes, functions can be used stand alone or added into
 * a graph.  The GUI deals with a graph.  Don't make the connections be stored by the functions
 * at all.  Instead, have all of that stored either by the graph or by the data nodes.
 * 
 * Instead of using DataSet and Stream, I could make it so that a DataSet is any
 * IndexedSeq[IndexedSeq[Vector[Double]]].  The Vector[Double] at the bottom serves as an element.
 * I can then either overload or use implicits for doing conversions wrapping things.  I could
 * make a DataSet and a Stream that are themselves supertypes of IndexSeq.  I think I need a
 * different naming scheme though.  Stream has other uses and connotations.  The DataSet should
 * be the lower level.  Maybe a DataGroup for the higher level.
 * 
 * I need to think about how to do a MovieFilter.  The model I have described has stateful builders
 * with non-stateful functions that they produce.  That is less than ideal for the MovieFilter though I
 * can probably make it work.  The Builder will need special routines for the things like the slider GUI
 * or exporting movies.  Those same functions need to be accessible from the REPL or a script as well.
 * 
 * @author Mark Lewis
 */

object ScalaVis {
    type DataElement=Vector[Double]
    type DataSet=IndexedSeq[DataElement]
    type IOGroup=IndexedSeq[DataSet]
    type IOList=IndexedSeq[IOGroup]
    type Element = (IOList,IOList)=>IOList

    def main(args:Array[String]) {
        
    }
    
    def frame(comp:Component) {
        val f=new Frame {
            contents=comp
        }
        f.pack
        f.visible=true
    }
    
    implicit def set2Group(set:DataSet):IOGroup = IndexedSeq(set)
    implicit def set2List(set:DataSet):IOList = IndexedSeq(IndexedSeq(set))
    implicit def group2List(group:IOGroup):IOList = IndexedSeq(group)
}