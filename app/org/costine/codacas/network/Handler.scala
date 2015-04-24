package org.costine.codacas.network

import java.nio._
import java.nio.channels._
import java.nio.charset._
import java.util.concurrent.ConcurrentLinkedQueue

import akka.actor.ActorRef

import scala.annotation.tailrec

/**
 * Manage a Java NIO socket channel.
 * 
 * Reactor will invoke "run" when a SelectionKey read "event" happens
 * 
 * Client will use "out" function to write to a concurrent unbounded queue. 
 * The output will eventually be sent to the socket when it is guaranteed not to block
 * 
 */
class Handler(_sel:Selector, _socket:SocketChannel, _receiver:ActorRef)
  extends Runnable {
 
  val MAXIN = 64*1024
  val MAXOUT = 64*1024
  
  val socket:SocketChannel = _socket
  val selector:Selector = _sel
  
  val receiver = _receiver
  
  val raddr = socket.socket.getInetAddress.getHostAddress
  val rport = socket.socket.getPort
  val laddr = socket.socket.getLocalAddress.getHostAddress
  val lport = socket.socket.getLocalPort
  
  socket.configureBlocking(false)
  val selectionKey:SelectionKey = socket.register(selector,0)
  selectionKey.attach(this)
  selectionKey.interestOps(SelectionKey.OP_READ)
  
  private val queue:ConcurrentLinkedQueue[Object] = new ConcurrentLinkedQueue
  
  val id = s"${raddr}:${rport}/${laddr}:${lport}"
  
  /**
   * Client will call this to schedule a String message to be written to the
   * channel
   */
  def out (_msg:String) = {
    queue.add(_msg)
    selectionKey.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE)
    selector.wakeup
  }
  
  /**
   * Client will call this to schedule a Shutdown message to be written to the 
   * channel.
   */
  def out (_msg:ShutdownMessage) = {
    queue.add(_msg)
    selectionKey.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE)
    selector.wakeup
  }

  /**
   * log a message to the console log
   */
  def log (_msg:String) = println (s"Handler ${this.hashCode }: ${id} ${_msg}")

  
  val input:ByteBuffer = ByteBuffer.allocate(MAXIN)
  
  private var boutput:ByteBuffer = null
  
  //val latin1:Charset = Charset.forName( "ISO-8859-1" )
  val latin1:Charset = Charset.forName( "US-ASCII" )
  val decoder:CharsetDecoder = latin1.newDecoder()
  val encoder:CharsetEncoder= latin1.newEncoder()
  
  /**
   * Is the underlying socket still open?
   */
  def isOpen:Boolean = socket.isOpen
  
  /**
   * Get bytes from the bytebuffer and send them as "character" messages to the
   * receiver Actor (usually a "Game")
   */
  def process(_n:Int) = {

    @tailrec
    def process(c: Int, h: Handler): Unit = {
       if (c > 0) {
         val b = input.get

         // send the character back to the Game receiver actor
         receiver ! (b.toChar,this)
         log(s"| ${b.toChar} (${b.toShort})")
         process(c-1,h)
       }
    }

    if (_n > 0) {
      input.rewind
      process(_n, this)
    }
	  input.clear
  }
  
  /**
   * This is invoked inline from the Reactor thread after it selects
   */
  def run () = {
    log("running")
    if (isOpen) {
	    try {
        // read if Readable
	    	if (selectionKey.isReadable) read
        // write anything in the queue if socket not closed
			  if (socket.socket.isClosed) close else send
	    }
	    catch {
	      case _ex:Exception =>
	        _ex.printStackTrace(Console.out)
	        close
	    }
    }
    else {
      log("socket closed")
      selectionKey.cancel
    }
    log("not running")
  }
  
  /**
   * close this Handler's underlying socket connection
   */
  def close = {
    log("client closed connection")
    closed
    try
      try {
        if (socket.isOpen) socket.close
      }
      catch {
        case _soex =>
          log("exception closing socket")
          _soex.printStackTrace(Console.out)
      }

    catch {
      case _cex =>
        _cex.printStackTrace(Console.out)
    }
  }
  
  /**
   * cancel filtering for anything from the underlying socket.
   */
  def closed = {
    try
      selectionKey.cancel
    catch {
      case _soex =>
        _soex.printStackTrace(Console.out)
    }
    finally receiver ! (new SocketClosedMessage(null), this)

  }
  
  /**
   * read and process a byte from the socket into the ByteBuffer
   */
  def read = {
    if (isOpen) {
      log("reading")
      try {
	      val _n = socket.read(input)
	      if (_n == -1) close else process(_n)
      }
      catch {
        case _ex:Exception =>
         _ex.printStackTrace(Console.out)
         close
      }
    }
    else {
      log("reading closed")
      closed
    }

  }
  
  /**
   * send bytes from the queue to the ByteBuffer
   * 
   * called when socket is guaranteed not to block.
   */
  def send = {
    if (isOpen) {
      log("sending")
      try {
        // deal with the existing buffer
        if (boutput != null) {
          if (boutput.hasRemaining) {
            socket.write(boutput)
            // if nothing remaining all done with this, so clear the buffer
            if (!boutput.hasRemaining) boutput = null
          }
        }
        // make a new buffer if there is something to write and no buffer
        else {
          if (queue.size > 0) {
            val _o = queue.remove()

            if (_o.isInstanceOf[String]) {
              val _msg = _o.asInstanceOf[String]
              val output = CharBuffer.wrap(_msg.toCharArray)
              boutput = encoder.encode(output)
              boutput.rewind
              selectionKey.interestOps(
                SelectionKey.OP_READ | SelectionKey.OP_WRITE
              )
            }

            else if (_o.isInstanceOf[ShutdownMessage]) {
              val _msg = _o.asInstanceOf[ShutdownMessage]
              val t = _msg.text
              if (t != null) {
                val output = CharBuffer.wrap(t.toCharArray)
                boutput = encoder.encode(output)
                boutput.rewind
                while (boutput.hasRemaining) socket.write(boutput)
              }
              val w = _msg.time
              if (w > 0) Thread.sleep(w)
              socket.close
            }

          }
          // nothing left in the queue so we go back into "just waiting
          // for input mode"
          else selectionKey.interestOps(SelectionKey.OP_READ)
        }

      }
      catch {
          case _ex:Exception =>
            _ex.printStackTrace(Console.out)
            close
      }
    }
    else {
      log("writing closed")
      closed
    }
  }

}
