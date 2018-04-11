package org.costine.codacas

abstract class Interaction(val source:UniverseObject, val target:UniverseObject) {
  def interact():Result 
}
