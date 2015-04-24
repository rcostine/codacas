package org.costine.codacas.renderers

import org.costine.codacas._
import scala.collection.mutable.ListBuffer

class CharacterArrayRenderer(_u:Universe, _s:Int, _sep:String, _lsep:String)
	extends CharacterRenderer (_u,_sep,_lsep) {

	val s = _s
	def size = s 
 
	def render:Array[Array[String]] = {
	  val a = Array.ofDim[String](size, size)
	  val xgridStep = universe.width / size
	  val ygridStep = universe.height / size
	  var _x:Double = 0
	  for (i <- 0 to (size - 1)) {
	    var _xe = _x + xgridStep
	    var _y:Double = 0
	    var r = new Array[String](size)
	    a(i) = r
	    for (j <- 0 to (size - 1)) {
	      var _ye = _y + ygridStep
       
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
       
	      _y+=ygridStep
	    }
	    _x+=xgridStep
	  }
	  a
	}
 

}
