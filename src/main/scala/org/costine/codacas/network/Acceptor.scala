package org.costine.codacas.network

import org.costine.codacas.messages.PlayerHandlerRegistrationMessage
import org.costine.codacas.system.Logging

import scala.util.{Failure, Success, Try}

class Acceptor(val reactor:Reactor) extends Runnable with Logging {

  def debug = false

  def logPrefix = "Acceptor"

  def run (): Unit = reactor.serverSocketChannelMaybe match {
    case Failure(t) => log("failed to accept new connection because the server socket channel failed to open", t)
    case Success(serverSocketChannel) =>
      Try {
        Option(serverSocketChannel.accept).fold {
          log("null connection received from acceptor")
        } { channel =>
          log(s"accepted new connection from ${channel.socket.getInetAddress}")
          reactor.selectorMaybe match {
            case Success(selector) =>
              val gameActor = reactor.receiver
              gameActor ! PlayerHandlerRegistrationMessage(new Handler(selector, channel, gameActor))
            case Failure(t) =>
              log("cannot create handler, selector not available",t)
          }
        }
      }.recover {
        case t => log("failed to accept new connection", t)
      } getOrElse()
    }



}
