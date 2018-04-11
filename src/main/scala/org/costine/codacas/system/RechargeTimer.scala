package org.costine.codacas.system

import akka.actor.ActorRef

/**
 * Inform the _receiver that it's time to do a recharge
 */
class RechargeTimer (interval:Long, receiver:ActorRef ) extends ActorTimer(interval,receiver) {}