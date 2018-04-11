package org.costine.codacas.interactions

import org.costine.codacas.Result

class PhaserResult(code:Int, message:String, val phaser:Phaser) extends Result(code,message) {}
