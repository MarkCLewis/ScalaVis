package edu.trinity.scalavis

import scala.collection.mutable.ArrayBuffer

class DataGroup(val sets:IndexedSeq[IndexedSeq[Vector[Double]]]) extends IndexedSeq[IndexedSeq[Vector[Double]]]{
    def this() = this(new ArrayBuffer[IndexedSeq[Vector[Double]]]()) 
    
    def apply(index:Int):IndexedSeq[Vector[Double]] = sets(index)
    def length:Int = sets.length
    
    def mapSets(f:Vector[Double]=>Vector[Double]):DataGroup = new DataGroup(sets.map(_.map(f)))
    def filterSets(f:Vector[Double]=>Boolean):DataGroup = new DataGroup(sets.map(_.filter(f)))
}