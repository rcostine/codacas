package org.costine.codacas.renderers
import org.costine.codacas.Universe

abstract class CharacterRenderer(val universe: Universe, val rowSep:String, val lineSep:String) {
  def render:Array[Array[String]]
 
  override def toString: String = render.map { _.mkString(rowSep)}.mkString(lineSep)
}
