package org.costine.codacas.system

import akka.actor.ActorRef

/**
 * This object is sent to the _receiver actor every _interval milliseconds
 * until interrupted.
 */
class ActorTimer (_interval:Long, _receiver:ActorRef )
  extends Runnable {

  val interval = _interval
  val receiver = _receiver
  
  def run () {
    while (!Thread.interrupted) {
      Thread.sleep(interval)
      receiver ! (this)
    }
  }
}

