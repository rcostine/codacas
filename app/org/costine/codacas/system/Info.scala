package org.costine.codacas.system

import akka.actor.ActorRef
import org.costine.codacas.actors._

/**
 * Base class static message data container.
 * 
 * Contains version and other credits, etc.
 */
class Info {

  def send (act: ActorRef) = {
    for(sn <- data) {
    	act ! ("msg", sn)
    }
  }
  
  /**
   * Override this in subclass to contain different data that will
   * be sent to the player actor.
   */
  def data = {
	  List(
			  "Codacas Resurrected",
			  "Version 1.0",
			  "For Juan, and all those Temple CIS folks who have taken their final phaser hit."
	  )
  }
}
