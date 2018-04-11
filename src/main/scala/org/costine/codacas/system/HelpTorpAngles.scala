package org.costine.codacas.system


object HelpTorpAngles {
	def apply(ship: Option[String] = None) : HelpTorpAngles = new HelpTorpAngles(ship)
}

/**
 * show the torp angle chart with the ship in the middle
 */
case class HelpTorpAngles(ship: Option[String] = None) extends Info {
  override def data = List(
		"Torp angles 0-360, 360 equals 0):",
		"",
		"          270",
		"           |",
		"           |",
		"180 <----- "+ ship.getOrElse("+") +" -----> 0",
		"           |",
		"           |",
		"          90"
	)
}
