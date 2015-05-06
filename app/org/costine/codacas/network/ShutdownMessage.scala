package org.costine.codacas.network

// sent by Play (aka activator) to the game when the system is stopped.
// Game will forward to all Players, who should then shutdown their sockets
case class GameShutdownMessage(text: String, time: Long)

class ShutdownMessage(_msg:String, _time:Long) {
  val text = _msg
  val time = _time
}
