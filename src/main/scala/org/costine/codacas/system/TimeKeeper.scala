package org.costine.codacas.system

import akka.actor.ActorRef

/**
 * The receiver Actor should match on this to do a timer-based
 * operation.
 */
class TimeKeeper (interval:Long, receiver:ActorRef ) extends ActorTimer(interval,receiver) {}
