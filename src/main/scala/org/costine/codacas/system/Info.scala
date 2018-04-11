package org.costine.codacas.system

import akka.actor.ActorRef
import org.costine.codacas.actors._
import org.costine.codacas.messages.TextMessage

/**
 * Base class static message data container.
 * 
 * Contains version and other credits, etc.
 */
class Info {

  def send (act: ActorRef): Unit = for (sn <- data) {act ! TextMessage(sn)}
  
  /**
   * Override this in subclass to contain different data that will
   * be sent to the player actor.
   */
  def data = List(
    "Codacas Resurrected",
    "Version 1.0.1",
    "For Juan Noyles, Joe Tait, and all those Temple CIS folks who have taken their final phaser hit."
  )
}
