package org.costine.codacas

import fi.utu.cs.physics.Point2

class CodacasUniverse (_w:Double, _h:Double) extends fi.utu.cs.physics.Universe ( fi.utu.cs.physics.Universe.INFINITE,
    new Point2 (0,0),
    new Point2 (_w,_h)){

  override def transpose (_p:Point2):Point2 = {
    var p_x = _p.getX
    var p_y = _p.getY
    
    val maxX = getMaximumCorner.getX
    val maxY = getMaximumCorner.getY
    
    val minX = getMinimumCorner.getX
    val minY = getMinimumCorner.getY
    
    if (p_x < minX) {
      p_x = maxX - (Math.abs(p_x) % (maxX - minX))
    }
    else if (p_x > maxX) {
      p_x = minX + (Math.abs(p_x) % (maxX - minX))
    }
    
    if (p_y < minY) {
      p_y = maxY - (Math.abs(p_y) % (maxY - minY))
    }
    else if (p_y > maxY) {
      p_y = minY + (Math.abs(p_y) % (maxY - minY))
    }
    
    new Point2(p_x,p_y)
  }
}
