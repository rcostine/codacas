package org.costine.codacas

import java.util.Random

class ObjectPlacer(_u:Universe) {
	private var u:Universe = _u
	private var generator:Random = new Random
 	
	def random = generator
 
 	def random (_r:Random ) = {
 	  generator=_r
 	}
  
	def randomDouble:Double = random.nextDouble()
 
   	def random(_min:Double, _max:Double) = {
  		if (_max == _min ) 
  			_max
  		else 
  			(Math.abs(_max - _min) * randomDouble) + Math.min(_max,_min)
  	}
    
  	def universe = u
	def universe (_u:Universe) = {
	  u = _u
	}
 
 	def place (uo:UniverseObject) = {
 		val s = uo.size
 		val x = random(s,universe.width - s)
  		val y = random(s,universe.height - s)
  		uo.location(new Coordinate(x,y))
  		universe.accept(uo);
 	}
 
}
