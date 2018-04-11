package org.costine.codacas.network

import java.nio._
import java.nio.channels._
import java.nio.charset._
import java.util.concurrent.ConcurrentLinkedQueue

import akka.actor.ActorRef
import org.costine.codacas.messages.CharacterFromHandlerMessage
import org.costine.codacas.messages.PlayerOffMessage
import org.costine.codacas.messages.{ShutdownMessage, SocketClosedMessage}
import org.costine.codacas.system.Logging

import scala.annotation.tailrec
import scala.util.Try

/**
 * Manage a Java NIO socket channel.
 * 
 * Reactor will invoke "run" when a SelectionKey read "event" happens
 * 
 * Client will use "out" function to write to a concurrent unbounded queue. 
 * The output will eventually be sent to the socket when it is guaranteed not to block
 * 
 */
class Handler(val selector:Selector, val socket:SocketChannel, val receiver:ActorRef)
  extends Runnable with Logging {

  def debug = false

  protected val MAXIN: Int = 64*1024
  protected val MAXOUT: Int = 64*1024
  
  private val raddr = socket.socket.getInetAddress.getHostAddress
  private val rport = socket.socket.getPort
  private val laddr = socket.socket.getLocalAddress.getHostAddress
  private val lport = socket.socket.getLocalPort
  
  socket.configureBlocking(false)
  private val selectionKey:SelectionKey = socket.register(selector,0)
  selectionKey.attach(this)
  selectionKey.interestOps(SelectionKey.OP_READ)
  
  private val queue:ConcurrentLinkedQueue[Object] = new ConcurrentLinkedQueue
  
  val id = s"$raddr:$rport/$laddr:$lport"
  
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
  def out (msg:ShutdownMessage): Selector = {
    queue.add(msg)
    selectionKey.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE)
    selector.wakeup
  }

  /**
   * log a message to the console log
   */
  def logPrefix = s"Handler ${this.hashCode }: $id"

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
  def process(n:Int, input: ByteBuffer): Unit = {

    @tailrec
    def process(c: Int): Unit = if (c > 0) {
      val b = input.get
      log(s"| got a '${b.toChar}' (${b.toShort}) from the input buffer")
      // send the character back to the Game receiver actor
      receiver ! CharacterFromHandlerMessage(b.toChar,this)
      process(c-1)
    }

    if (n > 0) {
      input.rewind
      process(n)
      ()
    } else {
      val buffer = input.clear
      close()
    }
  }
  
  /**
   * This is invoked inline from the Reactor thread after it selects
   */
  def run (): Unit = {
    log("running")
    if (isOpen) Try {
      // read if Readable
      if (selectionKey.isReadable) read()
      // write anything in the queue if socket not closed
      if (socket.socket.isClosed) close() else send()
    }.recover { case t =>
      log("failed to read from socket", t)
      close()
    }
    else {
      log("socket closed")
      selectionKey.cancel()
    }
    log("not running")
  }
  
  /**
   * close this Handler's underlying socket connection
   */
  def close(): Unit = {
    log("client closed connection")
    Try {
      if (socket.isOpen) socket.close()
    }.recover { case t => log("exception closing socket",t)}.foreach(_ => ())
  }
  
  /**
   * cancel filtering for anything from the underlying socket.
   */
  def closed(): Unit = {
    Try {
      selectionKey.cancel()
    }.recover { case t => log("socket close failed to cancel selection key",t)}.foreach {_ => ()}
    receiver ! PlayerOffMessage(SocketClosedMessage(new Exception("socket closed")),this)
  }
  
  /**
   * read and process a byte from the socket into the ByteBuffer
   */
  def read(): Unit = if (isOpen) {
      log("reading")
      Try {
        val input: ByteBuffer = ByteBuffer.allocate(MAXIN)
        process(socket.read(input),input)
        ()
      }.recover { case t => log("error reading from socket",t)}.foreach { _ => ()}
    }
    else {
      log("attempt to read closed socket")
      closed()
    }
  
  /**
   * send bytes from the queue to the ByteBuffer
   * 
   * called when socket is guaranteed not to block.
   */
  def send(): Unit =
    if (isOpen) {
      Try {
        log("sending")
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
            queue.remove() match {
              case msg: String =>
                val output = CharBuffer.wrap(msg.toCharArray)
                boutput = encoder.encode(output)
                boutput.rewind
                selectionKey.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE)
                ()
              case msg: ShutdownMessage =>
                val t = msg.text
                if (t != null) {
                  val output = CharBuffer.wrap(t.toCharArray)
                  boutput = encoder.encode(output)
                  boutput.rewind
                  while (boutput.hasRemaining) socket.write(boutput)
                }
                val w = msg.time
                if (w > 0) Thread.sleep(w)
                socket.close()
              case msg => ()
            }
          }
          // nothing left in the queue so we go back into "just waiting
          // for input mode"
          else selectionKey.interestOps(SelectionKey.OP_READ)
        }
      }.recover { case t =>
        log("socket sending failed",t)
        close ()
      }.foreach { _ => ()}
    }
    else {
      log("writing closed")
      closed()
    }

}
