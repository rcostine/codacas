package org.costine.codacas.interactions

import org.costine.codacas.Result

class PhaserResult(code:Int, message:String, p:Phaser) extends Result(code,message) {
  def phaser = p
  
}
