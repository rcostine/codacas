package org.costine.codacas.network

import java.net._
import java.nio._
import java.nio.channels._
import java.nio.channels.spi._

class Acceptor(_reactor:Reactor) extends Runnable {
  
  val reactor = _reactor
  
  def log (_msg:String) = {
    println ("Acceptor: " + _msg)
  }
  
  def run () = {
    try {
      val _channel = reactor.serverSocketChannel.accept
      if (_channel != null) {
    	  
    	  log("accepted new connection from " + _channel.socket.getInetAddress )
    	  reactor.receiver ! (new Handler (reactor.selector,_channel,reactor.receiver))
    	  
      }
    }
    catch {
      case _ex:Exception =>
        _ex.printStackTrace(Console.out)
    }
  }
}
