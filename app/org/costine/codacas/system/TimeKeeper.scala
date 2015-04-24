package org.costine.codacas.system

import java.lang.Runnable

import akka.actor.ActorRef

/**
 * The receiver Actor should match on this to do a timer-based
 * operation.
 */
class TimeKeeper (_interval:Long, _receiver:ActorRef )
  extends ActorTimer(_interval,_receiver) {
}
