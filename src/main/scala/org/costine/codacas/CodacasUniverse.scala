package org.costine.codacas

import fi.utu.cs.physics.Point2
import fi.utu.cs.physics.Point2.ORIGIN
import fi.utu.cs.physics.Universe.INFINITE

object CodacasUniverse {
  def transpose (pt: Double, minVal: Double, maxVal: Double): Double =
    if (pt < minVal) maxVal - (Math.abs(pt) % (maxVal - minVal))
    else if (pt > maxVal) minVal + (Math.abs(pt) % (maxVal - minVal))
    else pt
}

class CodacasUniverse (w:Double, h:Double)
  extends fi.utu.cs.physics.Universe (INFINITE, ORIGIN, new Point2 (w,h)){

  override def transpose (p:Point2): Point2 =
    new Point2(
      CodacasUniverse.transpose(p.getX,getMinimumCorner.getX,getMaximumCorner.getX),
      CodacasUniverse.transpose(p.getY,getMinimumCorner.getY,getMaximumCorner.getY)
    )
}
