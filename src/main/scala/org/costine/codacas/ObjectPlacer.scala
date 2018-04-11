package org.costine.codacas

import java.util.Random

class ObjectPlacer(_u:Universe) {
	private var u:Universe = _u
	private var generator:Random = new Random
 	
	def random: Random = generator
 
 	def random (_r:Random ): Unit = generator=_r
  
	def randomDouble:Double = random.nextDouble()
 
  def random(_min:Double, _max:Double): Double =
    if (_max == _min ) _max
    else (Math.abs(_max - _min) * randomDouble) + Math.min(_max,_min)
    
	def universe: Universe = u

	def universe (_u:Universe): Unit = u = _u

 	def place (uo:UniverseObject): Boolean = {
    uo.location(Coordinate.random(uo,this))
    universe.accept(uo)
 	}
 
}
