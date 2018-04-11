package org.costine.codacas

import fi.utu.cs.physics.Vector2

class Velocity (val speed:Double, val timeUnits :Long, val angle:Double) {

  def isStopped ():Boolean = speed == 0
  
  def stop () = new Velocity (0, timeUnits, angle)
  
  def velocity: Vector2 = new fi.utu.cs.physics.Vector2 (
    Math.sin(Math.toRadians(angle)) * speed,
    Math.cos(Math.toRadians(angle)) * speed
  )
  
}
