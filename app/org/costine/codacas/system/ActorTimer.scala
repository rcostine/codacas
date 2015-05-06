package org.costine.codacas.system

import akka.actor.ActorRef

/**
 * This object is sent to the _receiver actor every _interval milliseconds
 * until interrupted.
 */
class ActorTimer (_interval:Long, _receiver:ActorRef )
  extends Runnable with Logging {

  def debug = false

  def logPrefix = s"${getClass.getName}"

  val interval = _interval
  val receiver = _receiver

  // true - continue, false - stop
  def sleep : Boolean = {
    try {
      Thread.sleep(interval)
      receiver ! (this)
      !Thread.currentThread().isInterrupted()
    }
    catch {
      case e : InterruptedException => {
        log(e.getMessage)
        false
      }
    }
  }

  def run () {
    while (sleep) {}
    log(s"${getClass.getName} ended")
  }
}

