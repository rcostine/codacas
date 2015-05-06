package org.costine.codacas.network

import java.lang.Runnable
import java.lang.Thread
import java.net._
import java.nio._
import java.nio.channels._
import java.nio.channels.spi._
import akka.actor.ActorRef
import org.costine.codacas.system.Logging

class Reactor(_port:Int, _receiver:ActorRef ) extends Runnable with Logging {

	def debug = false

	def logPrefix = "Reactor"

  val port = _port
  val receiver = _receiver
  val selector:Selector = Selector.open
  val serverSocketChannel:ServerSocketChannel = ServerSocketChannel.open
  
  log ("binding port "  + port)

  serverSocketChannel.socket.bind(new InetSocketAddress(port))
  serverSocketChannel.configureBlocking(false)
  attachAcceptKey(serverSocketChannel)

  def attachAcceptKey (_sc:ServerSocketChannel) = {
     val _sk = _sc.register(selector,SelectionKey.OP_ACCEPT)
     _sk.attach(new Acceptor (this))
  }


  
  def dispatch (_sk:SelectionKey) = {
	  val _a = _sk.attachment
	  if (_a.isInstanceOf[Runnable]) {
	    log ("dispatching " + _a.getClass)
	    _a.asInstanceOf[Runnable].run
	  }
  }
  
  def run () = {
    if (serverSocketChannel != null) {
	    while (!Thread.interrupted) {
	      log("selecting")
	      selector.select
      
	      val selected = selector.selectedKeys
	      val selIter = selected.iterator

	      log("selected " + selected.size + " keys")
	      
	      if (selected.size > 0) {
		      while (selIter.hasNext) {
			        val o = selIter.next
			        if (o.isInstanceOf [SelectionKey]) {
			          val _sk = o.asInstanceOf[SelectionKey]
			          dispatch(_sk)
			        }
		      }
		      selected.clear
	      }
	    }
    }
  	else {
  		log ("failed to bind server socket")
  	}
  }

  
}
