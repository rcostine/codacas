package org.costine.codacas

class HyperSpacer (uo:Universe) extends ObjectPlacer (uo) {
  def place (ship:Ship): Boolean = universe.ships().find { _.name == ship.name} match {
    case Some(_) => super.place(ship)
    case None => false
  }
}
