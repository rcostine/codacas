package org.costine.codacas.renderers

import org.costine.codacas.Universe

abstract class CharacterRenderer(_u:Universe, _sep:String, _lsep:String) {

  	val u = _u
	val ss = _sep
	val ls = _lsep
	
	def universe = u
	def sep = ss
	def lsep = ls
  
  def render:Array[Array[String]]
 
  override def toString = {
		var buf:StringBuffer = new StringBuffer 
		for ( line <- render ) {
		
		  if (buf.length > 0) {
		    buf.append(lsep) 
		  }
		  
		  var lbuf:StringBuffer = new StringBuffer
		  for (ele <- line) {
		    if (lbuf.length > 0) {
		      lbuf.append(sep)
		    }
		    lbuf.append(ele)
		  }
		  buf.append(lbuf.toString)  
		}
		buf.toString
	}
}
