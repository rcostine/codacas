package org.costine.codacas.system

/**
 * show the torp angle chart with the ship in the middle
 */
class HelpTorpAngles extends Info {
  
  var ship:String = "+"
  
  override def data = {
	if (ship == null) {
		ship="+"
	}
	List(
			  "Torp angles 0-360, 360 equals 0):",
			  "",
			  "          270",
			  "           |",
			  "           |",
			  "180 <----- "+ ship +" -----> 0",
			  "           |",
			  "           |",
			  "          90"
	  )
  }
}
