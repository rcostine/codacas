package org.costine.codacas.network

import java.net._
import java.nio.channels._
import akka.actor.ActorRef
import org.costine.codacas.system.Logging
import scala.util.{Failure, Success, Try}

class Reactor(val port:Int, val receiver:ActorRef ) extends Runnable with Logging {

	def debug = false
	def logPrefix = "Reactor"

  val selectorMaybe: Try[Selector] = Try { Selector.open }
  val serverSocketChannelMaybe: Try[ServerSocketChannel] = Try { ServerSocketChannel.open }
  def badSelectionKey(sc: ServerSocketChannel): SelectionKey= new SelectionKey {
    override def cancel(): Unit = ???
    override def isValid: Boolean = false
    override def channel(): SelectableChannel = sc
    override def readyOps(): Int = ???
    override def interestOps(): Int = ???
    override def interestOps(ops: Int): SelectionKey = ???
    override def selector(): Selector = ???
  }
  
  log (s"binding port $port")
  serverSocketChannelMaybe.foreach { serverSocketChannel =>
    serverSocketChannel.socket.bind(new InetSocketAddress(port))
    serverSocketChannel.configureBlocking(false)
    attachAcceptKey(serverSocketChannel)
  }

  def attachAcceptKey (sc:ServerSocketChannel): AnyRef =
    selectorMaybe match {
      case Success(selector) =>
        sc.register(selector,SelectionKey.OP_ACCEPT).attach(new Acceptor (this))
      case Failure(t) =>
        log("can't attach accept key for server socket channel, selector not available", t)
        badSelectionKey(sc)
    }

  def dispatch (sk:SelectionKey): Unit = sk.attachment match {
    case a: Runnable =>
      log (s"dispatching ${a.getClass}" )
      a.run()
    case x =>
      log (s"cannot dispatch ${x.getClass}; not Runnable" )
  }

  def select(selector: Selector) : Unit = {
    log("selecting")
    selector.select

    val selected = selector.selectedKeys
    val selIter = selected.iterator

    log("selected " + selected.size + " keys")

    if (selected.size > 0) {
      while (selIter.hasNext) {
        val o = selIter.next
        o.attachment match {
          case a: Runnable =>
            log(s"dispatching ${a.getClass}")
            a.run()
          case x =>
            log(s"cannot dispatch ${x.getClass}; not Runnable")
        }
      }
      selected.clear
    }
  }

  def run (): Unit = serverSocketChannelMaybe.map { serverSocketChannel =>
	    while (!Thread.interrupted)
        selectorMaybe match {
          case Success(selector) => select(selector)
            /**
            log("selecting")
            selector.select
            val selected = selector.selectedKeys
            log(s"selected ${selected.size} keys")
            import scala.collection.JavaConverters._
            selected.iterator().asScala.collect {case sk: SelectionKey =>
              dispatch(sk)
            }
            selected.clear()
              */
          case Failure(t) =>
            log("cannot select from socket - selector is not open",t)
        }
    } getOrElse {
  		log ("failed to bind server socket")
  	}
}
