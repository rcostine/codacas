package org.costine.codacas.renderers

import scala.collection.mutable.ListBuffer
import org.costine.codacas.Ship
import org.costine.codacas.Star
import org.costine.codacas.Torp

class ShortRangeCharacterArrayRenderer(
		_ship:Ship,
		_extent:Double,
		_s:Int,
		_sep:String,
		_lsep:String)
	extends CharacterRenderer (_ship.universe,_sep,_lsep){

  
  val s = _s
  val ex = _extent
  
  // the ship around which to scan
  def ship = _ship
  
  // number of grid elements on a side 
  def size = s
  
  // extent of the scan in real units
  def extent = ex
  
  def render:Array[Array[String]] = {
	  
	  val center = extent/2            
	  val a = Array.ofDim[String](size,size)
	  val gridStep = extent / size
   
      var _x = universe.wrapWidth(ship.location.x - center)
	  
      for (i <- 0 to (size - 1)) {
	    var _xe = universe.wrapWidth(_x + gridStep)

	    var r = new Array[String](size)
	    a(i) = r
	    var _y = universe.wrapWidth(ship.location.y - center)
	    for (j <- 0 to (size - 1)) {
	      var _ye = universe.wrapHeight(_y + gridStep)

	      val objs = universe.locateObjectsInGrid(_x, _xe, _y, _ye)
	      if (objs.size == 0) {
	        r(j) = "."
	      }
	      else {
	    	  var ships: ListBuffer[Ship] = new ListBuffer[Ship]
	    	  var stars: ListBuffer[Star] = new ListBuffer[Star]
	    	  var torps: ListBuffer[Torp] = new ListBuffer[Torp]                                             
	    	  for (obj <- objs) {
	    	    if (obj.isInstanceOf[Ship]) {
	    	      ships+=obj.asInstanceOf[Ship]                             
	    	    }
	    	    else if (obj.isInstanceOf[Star]) {
	    	      stars+=obj.asInstanceOf[Star]
	    	    }
	    	    else if (obj.isInstanceOf[Torp]) {
	    	      torps+=obj.asInstanceOf[Torp]
	    	    }
	    	  }
        
	    	  if (ships.size == 1) {
	    	    val who = ships.apply(0)
	    	    r(j) = who.name.substring(0,1)
	    	  }
	    	  else if (ships.size > 1) {
	    	    r(j) = "@"
	    	  }
	    	  else if (stars.size > 0) {
	    	    r(j) = "*"
	    	  }
	    	  else if (torps.size > 0) {
	    	    r(j) = "+"
	    	  }
	    	  else {
	    		r(j) = "."
	    	  } 
	      }
       
	      _y=universe.wrapHeight(_y + gridStep)
	    }
	    _x=universe.wrapWidth(_x + gridStep)
	  }
	  a
	}
}
