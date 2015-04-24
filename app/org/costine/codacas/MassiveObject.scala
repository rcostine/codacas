package org.costine.codacas

abstract class MassiveObject (m:Double, s:Double, loc:Coordinate, u:Universe)
  extends UniverseObject(s:Double, m:Double, loc:Coordinate, u:Universe) {
  def mass:Double = m
}
