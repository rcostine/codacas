package org.costine.codacas.interactions

import org.costine.codacas.Interaction
import org.costine.codacas.Ship
import org.costine.codacas.Result

class Phaser (_from:Ship, _to:Ship, _fooples:Int) extends Interaction(_from, _to) {

  val PHASER_SAMESHIP 		= 1
  val PHASER_INSUFFICIENT	= 2 
  val PHASER_OK				= 0
  
  def from = _from
  def to = _to
  def fooples = _fooples
  
  var effective = 0
  
  def interact():Result = {
    if (from.name == to.name ) {
      new PhaserResult (PHASER_SAMESHIP,"Cannot phaser yourself",this)
    }
    else {
      val expendedFooples = fooples
      if (from.fooples < expendedFooples) {
        new PhaserResult (PHASER_INSUFFICIENT,"Insufficient energy",this)
      }
      else {
        effective = to.phaserHit(from,fooples)
        new PhaserResult (PHASER_OK,"Phasers fired",this)
      }
    }
  }
  
}
