package edu.trinity.scalavis

import scala.swing.Component

/**
 * This trait is for the classes that build elements.  It also has the code
 * for constructing whatever GUI elements are needed.
 * 
 * @author Mark Lewis
 */
trait Builder {
    def buildElement():ScalaVis.Element
    def numXInputs:Int
    def numYInputs:Int
    def numOutputs:Int
    def component:Component
}