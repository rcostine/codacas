package org.costine.codacas

import scala.collection.mutable.ListBuffer
import java.util.Random
import java.lang.Math

class StarFactory(_ratio:Double, 
                  _minStarSize:Double, _maxStarSize:Double,
                  _minStarMass:Double, _maxStarMass:Double,
                  _wqlRR:Double,_wqlMax:Double
					) {

	private var u:Option[Universe] = None
	private var ratio:Double = _ratio
	private var minStarSize:Double = _minStarSize
	private var maxStarSize:Double = _maxStarSize
 	private var minStarMass:Double = _minStarMass
	private var maxStarMass:Double = _maxStarMass
  private var wql_RR:Double = _wqlRR
	private var wql_Max:Double = _wqlMax
	private var generator:Random = new Random
 
	private var starList:ListBuffer[Star] = new ListBuffer[Star]
 
	def min: Double = minStarSize
	def min(_min:Double): Unit = minStarSize = _min
 
	def max: Double = maxStarSize
 	def max(_max:Double): Unit = maxStarSize = _max
  
 	def minMass: Double = minStarMass
	def minMass(_min:Double): Unit = minStarMass = _min
 
	def maxMass: Double = maxStarMass
 	def maxMass(_max:Double): Unit = maxStarMass = _max
 
	def wqlMax: Double = wql_Max
 	def wqlMax (_max:Double): Unit = wql_Max = _max
   
	def wqlRR: Double = wql_RR
 	def wqlRR(_rr:Double): Unit = wql_RR = _rr

	def desiredDensity: Double = ratio
	def desiredDensity (_ratio:Double): Unit = ratio = _ratio
 
 	def universe: Option[Universe] = u

	def universe (_u:Universe): Unit = u = Some(_u)

 	def stars: ListBuffer[Star] = starList
 
 	def random: Random = generator
 
 	def random (_r:Random ): Unit = generator=_r
  
	def randomDouble: Double = random.nextDouble()
 
 	def totalStarArea: Double = starList.toList.map{ _.area()}.sum

	def density: Double = universe.map {totalStarArea / _.universearea}.getOrElse(0)

	def makeStars (): Unit = while (density < desiredDensity) { newStar() }
  		
  def random(_min:Double, _max:Double): Double =
    if (_max == _min ) _max
    else (Math.abs(_max - _min) * randomDouble) + Math.min(_max,_min)

 
  def newStar(): ListBuffer[Star] = universe.map { uu =>
    val s = random(min, max)
    val m = random(minMass, maxMass)
    val x = random(s, uu.width - s)
    val y = random(s, uu.height - s)

    val star = new Star(wqlRR, wqlMax, m, s, new Coordinate(x, y), uu)
    star.setAcceleration(0, 0)
    star.setVelocity(0, 0)
    uu.place(star)
    stars += star
  }.getOrElse(stars)


  def get (): Unit = universe.foreach { uu =>
    for (star <- stars) { uu.add(star) }
  }
}
