package org.costine.codacas

import fi.utu.cs.physics.Point2

object Coordinate {
  def random(uo: UniverseObject, placer:ObjectPlacer): Coordinate =
      Coordinate(
        placer.random(uo.size,placer.universe.width - uo.size),
        placer.random(uo.size,placer.universe.height - uo.size)
      )

  def apply (x: Double, y: Double): Coordinate = new Coordinate(x,y)

  def apply (point: Point2): Coordinate = new Coordinate(point.getX,point.getY)


}


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

      def dist (xd: Double,yd: Double) : Double =  Math.sqrt((xd * xd) + (yd * yd))

      // first the standard distance formula above - no wrap
      def dist1 = distance(target)

      // the distance for wrap in the y direction only
      // need to put the point with least Y coordinate in x1,y1
      def dist2 =
        (if (y > target.y) (target.y, y, target.x, x)
          else (y, target.y, x, target.x)) match {
            case (y1,y2,x1,x2) => dist(x2 - x1,(u.height - y2) + y1)
        }

      // the distance for wrap in the x direction only
      // need to put the point with least X coordinate in x1,y1
      def dist3 =
        (if (x > target.x) (target.y, y, target.x, x)
        else (y,target.y,x,target.x)) match {
          case (y1,y2,x1,x2) => dist((u.width - x2) + x1,y2 - y1)
        }

      // the distance for wrap in the x and y directions
      // need to put the point with least Y coordinate in x1,y1
      def dist4 =
        (if (y > target.y) (target.y, y, target.x, x)
        else (y, target.y, x, target.x)) match {
          case (y1,y2,x1,x2) =>
            dist(if (x2 > x1) (u.width - x2) + x1 else (u.width - x1) + x2,(u.height - y2) + y1)
        }

      // our distance is the min of the four possible distances
      List(dist1,dist2,dist3,dist4).min
    }

  def snap = new Coordinate(xLoc,yLoc)

}
