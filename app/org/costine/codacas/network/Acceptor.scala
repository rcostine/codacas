package org.costine.codacas.network

import java.net._
import java.nio._
import java.nio.channels._
import java.nio.channels.spi._

import org.costine.codacas.system.Logging

class Acceptor(_reactor:Reactor) extends Runnable with Logging {

  def debug = false

  val reactor = _reactor
  
  def logPrefix = "Acceptor"
  
  def run () = {
    try {
      val _channel = reactor.serverSocketChannel.accept
      if (_channel != null) {
    	  
    	  log(s"accepted new connection from ${_channel.socket.getInetAddress}" )
    	  reactor.receiver ! (new Handler (reactor.selector,_channel,reactor.receiver))
    	  
      }
    }
    catch {
      case _ex:Exception =>
        _ex.printStackTrace(Console.out)
    }
  }
}
