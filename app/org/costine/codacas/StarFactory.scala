package org.costine.codacas

import scala.collection.mutable.ListBuffer
import java.util.Random
import java.lang.Math

class StarFactory(_ratio:Double, 
                  _minStarSize:Double, _maxStarSize:Double,
                  _minStarMass:Double, _maxStarMass:Double,
                  _wqlRR:Double,_wqlMax:Double
					) {

	private var u:Universe = null
	private var ratio:Double = _ratio
	private var minStarSize:Double = _minStarSize
	private var maxStarSize:Double = _maxStarSize
 	private var minStarMass:Double = _minStarMass
	private var maxStarMass:Double = _maxStarMass
  	private var wql_RR:Double = _wqlRR
	private var wql_Max:Double = _wqlMax
	private var generator:Random = new Random
 
	private var starList:ListBuffer[Star] = new ListBuffer[Star]
 
	def min = minStarSize
	def min(_min:Double) = {
	  minStarSize = _min
	}
 
	def max = maxStarSize
 	def max(_max:Double) = {
	  maxStarSize = _max
	}
  
 	def minMass = minStarMass
	def minMass(_min:Double) = {
	  minStarMass = _min
	}
 
	def maxMass = maxStarMass
 	def maxMass(_max:Double) = {
	  maxStarMass = _max
	}
 
	def wqlMax = wql_Max
 	def wqlMax (_max:Double) = {
	  wql_Max = _max
	}
   
	def wqlRR = wql_RR
 	def wqlRR(_rr:Double) = {
	  wql_RR = _rr
	}
  
	def desiredDensity = ratio
	def desiredDensity (_ratio:Double) = {
	  ratio = _ratio
	}
 
 	def universe = u
	def universe (_u:Universe) = {
	  u = _u
	}
 
 	def stars = starList
 
 	def random = generator
 
 	def random (_r:Random ) = {
 	  generator=_r
 	}
  
	def randomDouble:Double = random.nextDouble()
 
 	def totalStarArea = {
		var t:Double = 0
		for (star <- starList) {
			t+=star.area
		}
  		t
 	}
 
	def density:Double = {
	  totalStarArea / universe.universearea
	}
 
	def makeStars () = {
		while (density < desiredDensity) {
		  newStar
		}
	}
  		
  	def random(_min:Double, _max:Double) = {
  		if (_max == _min ) 
  			_max
  		else 
  			(Math.abs(_max - _min) * randomDouble) + Math.min(_max,_min)
  	}
 
  	def newStar () = {
  		val s = random(min,max)
  		val m = random(minMass, maxMass)
  		val x = random(s,universe.width - s)
  		val y = random(s,universe.height - s)
  		
  		val star = new Star(wqlRR, wqlMax, m, s, new Coordinate (x,y), universe)
  		star.setAcceleration(0,0)
  		star.setVelocity(0,0)
  		universe.place(star)
  		stars += star
  	}

  	def get () = {
  	  for (star <- stars) {
  	    universe.add(star)
  	  }
  	}
  
}
