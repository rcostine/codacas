package org.costine.codacas

import scala.collection.mutable.ListBuffer

class HyperSpacer (uo:Universe) extends ObjectPlacer (uo) {
  def place (_s:Ship) = {
	  var l:ListBuffer[Ship] = new ListBuffer[Ship]
	  for (ship <- universe.ships) {
	    if (ship.name == _s.name ) {
	      l += ship
	    }
	  }
   
   	  if (l.size > 0)
   		  super.place(_s)
      else
    	  false	// ship not found
  }
}
