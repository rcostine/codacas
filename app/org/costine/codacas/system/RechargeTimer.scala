package org.costine.codacas.system

import akka.actor.ActorRef

/**
 * Inform the _receiver that it's time to do a recharge
 */
class RechargeTimer (_interval:Long, _receiver:ActorRef )
  extends ActorTimer(_interval,_receiver) {
}