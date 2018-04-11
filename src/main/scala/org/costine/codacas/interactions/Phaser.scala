package org.costine.codacas.interactions

import org.costine.codacas.Interaction
import org.costine.codacas.Ship
import org.costine.codacas.Result

object Phaser {
  val PHASER_SAMESHIP 		= 1
  val PHASER_INSUFFICIENT	= 2
  val PHASER_OK				= 0
}


class Phaser (val from:Ship, val to:Ship, val fooples:Int) extends Interaction(from, to) {
  var effective = 0
  
  def interact():Result = {
    if (from.name == to.name )
      new PhaserResult (Phaser.PHASER_SAMESHIP,"Cannot phaser yourself",this)
    else {
      val expendedFooples = fooples
      if (from.fooples < expendedFooples)
        new PhaserResult (Phaser.PHASER_INSUFFICIENT,"Insufficient energy",this)
      else {
        effective = to.phaserHit(from,fooples)
        new PhaserResult (Phaser.PHASER_OK,"Phasers fired",this)
      }
    }
  }
  
}
