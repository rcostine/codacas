package org.costine.codacas.system

import akka.actor.ActorRef

import scala.util.{Success, Failure, Try}

/**
 * This object is sent to the _receiver actor every _interval milliseconds
 * until interrupted.
 */
class ActorTimer (val interval:Long, val receiver:ActorRef )
  extends Runnable with Logging {

  def debug = false

  def logPrefix = s"${getClass.getName}"

  // true - continue, false - stop
  def sleep : Boolean = Try {
      Thread.sleep(interval)
      receiver ! this
      !Thread.currentThread().isInterrupted
    } match {
      case Success(b) => b
      case Failure(e) => e match {
        case ix: InterruptedException =>
          log(e.getMessage)
          false
        case other =>
          log(e.getMessage)
          true
      }
    }

  def run () {
    while (sleep) {}
    log(s"${getClass.getName} ended")
  }
}

