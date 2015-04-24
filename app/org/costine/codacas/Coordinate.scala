package org.costine.codacas

import fi.utu.cs.physics.Point2

class Coordinate(xLoc:Double, yLoc:Double) extends Point2(xLoc, yLoc) {
    def x:Double = super.getX
    def y:Double = super.getY
    
    def distance (target:Coordinate):Double = {
      val xd = target.x - x
      val yd = target.y - y    
      Math.sqrt((xd * xd) + (yd * yd))
    }
    
    // distance in wraparound universe is the smallest of the 4 possibilities
    def distance (target:Coordinate,u:Universe):Double = {
      // first the standard distance formula - no wrap
      val d = new Array[Double](4)
      var xd = target.x - x
      var yd = target.y - y
      d(0) = Math.sqrt((xd * xd) + (yd * yd))
      
      // then we try the distance for wrap in the y direction only
      // need to put the point with least Y coordinate in x1,y1
      var y1 = y
      var y2 = target.y
      var x1 = x
      var x2 = target.x

      if (y > target.y) {
    	  y1 = target.y
    	  x1 = target.x
    	  y2 = y
    	  x2 = x
      }
      xd = x2 - x1
      yd = (u.height - y2) + y1
      d(1) = Math.sqrt((xd * xd) + (yd * yd))
      
      // then we try the distance for wrap in the x direction only
      // need to put the point with least X coordinate in x1,y1      
      y1 = y
      y2 = target.y
      x1 = x
      x2 = target.x
      if (x > target.x) {
    	  y1 = target.y
    	  x1 = target.x
    	  y2 = y
    	  x2 = x
      }
      yd = y2 - y1
      xd = (u.width - x2) + x1
      d(2) = Math.sqrt((xd * xd) + (yd * yd))
      
      // then we try the distance for wrap in the x and y directions
      // need to put the point with least Y coordinate in x1,y1
      y1 = y
      y2 = target.y
      x1 = x
      x2 = target.x
      if (y > target.y) {
          y1 = target.y
    	  x1 = target.x
    	  y2 = y
    	  x2 = x
      }
      yd = (u.height - y2) + y1
      // then deal with wrapping the x coordinate distance
      if (x2 > x1) {
        xd = (u.width - x2) + x1
      }
      else {
        xd = (u.width - x1) + x2
      }
      d(3) = Math.sqrt((xd * xd) + (yd * yd))
      
      // get the smallest one
      var result = d(0)
      var i = 1
      while (i < d.length) {
        if (d(i) < result) {
          result = d(i)
        }
        i = i + 1
      }
      result
    }
}
