package org.costine.codacas.renderers

import org.costine.codacas._
import scala.collection.mutable.ListBuffer

class CharacterArrayRenderer(universe: Universe, size: Int, _sep:String, _lsep:String)
	extends CharacterRenderer (universe,_sep,_lsep) with GridSectionRenderer {

	def render: Array[Array[String]] =
    renderGrid(
      universe,
      size,
      () => 0,
      () => 0,
      (s: Int) => universe.width / s,
      (s: Int) => universe.height / s,
      (x: Double,xInc: Double) => x+xInc,
      (y: Double,yInc: Double) => y+yInc,
      GridSectionRenderer.defaultElementRenderer
    )

}
