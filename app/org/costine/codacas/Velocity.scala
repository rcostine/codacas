package org.costine.codacas

class Velocity (_s:Double, _u:Long, _d:Double) {
  private var s:Double = _s
  private var u:Long = _u
  private var dir:Double = _d
  
  def speed = s
  def timeUnits = u
  def angle = dir
  
  def isStopped ():Boolean = {
    speed == 0
  }
  
  def stop () = {
    s = 0
  }
  
  def velocity = {
    new fi.utu.cs.physics.Vector2 (
    		Math.sin(Math.toRadians(angle)) * speed,
    		Math.cos(Math.toRadians(angle)) * speed
    )
  }
  
}
