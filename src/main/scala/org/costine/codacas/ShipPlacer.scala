package org.costine.codacas

class ShipPlacer (uo:Universe) extends ObjectPlacer (uo){

  def place (ship:Ship): Boolean = {
		val l = universe.ships().filter { _.name == ship.name }
		if (l.isEmpty) super.place(ship) else false
  }
  
}
