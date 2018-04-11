package org.costine.codacas.renderers

import org.costine.codacas.Ship

class ShortRangeCharacterArrayRenderer(
		val ship:Ship, val extent:Double, val size:Int, sep:String, lsep:String)
	extends CharacterRenderer (ship.universe,sep,lsep) with GridSectionRenderer {

  
  def render:Array[Array[String]] = {
	  val center = extent/2

    renderGrid(
      universe,
      size,
      () => universe.wrapWidth(ship.location.x - center),
      () => universe.wrapHeight(ship.location.y - center),
      (s: Int) => extent/s,
      (s: Int) => extent/s,
      (lowX: Double, xGrid : Double) => universe.wrapWidth(lowX + xGrid),
      (lowY: Double, yGrid : Double) => universe.wrapHeight(lowY + yGrid),
      GridSectionRenderer.defaultElementRenderer
    )
	}
}
