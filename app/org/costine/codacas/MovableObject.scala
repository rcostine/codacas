package org.costine.codacas

import java.util.Date
import fi.utu.cs.physics.FrictionalBody

abstract class MovableObject(v:Velocity, m:Double, s:Double, loc:Coordinate, u:Universe) extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe){

  var lastMoved:Date = new Date
  var velocity = v
/** 
  def move ():Double = {
    val now = new Date
    val td = now.getTime - lastMoved.getTime
  
    val timeDiff = td / velocity.timeUnits
    val speed = velocity.speed
    
    val spaceDiff = timeDiff * speed
    
    val rad = Math.toRadians(velocity.angle)
    var newX = location.x + (Math.cos(rad) * spaceDiff)
    var newY = location.y + (Math.sin(rad) * spaceDiff)
    
    if (newX < 0) {
      newX = universe.width - (newX % universe.width)
    }
    if (newY < 0) {
      newY = universe.height - (newY % universe.height)
    }
    if (newX > universe.width) {
      newX = (newX - universe.width) % universe.width
    }
    if (newY > universe.height) {
      newY = (newY - universe.height) % universe.height
    }
    
    var newCoord = new Coordinate (newX,newY)
    val _b = new PlacingBody (s,newCoord)
    while (overlaps(newCoord,s)) {
      newCoord = new Coordinate (newX+size,newY+size)
    }
    location (newCoord)
    
    lastMoved = now
    spaceDiff
  }
*/
}
