package org.costine.codacas

import fi.utu.cs.physics.FrictionalBody
import fi.utu.cs.physics.Body
import fi.utu.cs.physics.Friction
import java.lang.Math


abstract class UniverseObject(_s: Double,_m: Double, _loc:Coordinate, _u:Universe)
  extends Body(_m, _s, _loc ) {
  
  def size:Double = super.getRadius
  
  def location:Coordinate = {
    val p = super.getPosition
    new Coordinate (p.getX,p.getY)
  }
  
  def universe:Universe = _u
  
  def location (_loc:Coordinate) {
    super.setPosition (_loc)
  }

  def area():Double = {
    Math.PI * (size * size)
  }
  
  def recharge()
  
  def distance (target:UniverseObject):Double = {
    location.distance(target.location,universe)
  }
   
}
