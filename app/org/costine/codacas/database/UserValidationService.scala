package org.costine.codacas.database

import akka.actor.ActorRef
import java.util.concurrent.LinkedBlockingQueue

import org.costine.codacas.system.Logging

case class ShutdownMessage (_msg:String, _time:Long)

class UserValidationService extends Runnable with Logging {

  def debug = false

	private val queue:LinkedBlockingQueue[Object] = new LinkedBlockingQueue
  
  
  def scheduleValidation (_user:User, _actor:ActorRef) = {
		queue.put(UserValidationRequest(_user,_actor,"unchecked"))
  }
  
  def logPrefix = "UserValidationService"
  
  /**
   * do a database dip here to look up whether user is valid
   */
  def validate (_user:User) = {
    	UserValidationResponse(_user,     
    	    _user.name match {
    	    		case "rcostine" => 
    	    			"valid"
    	    		case _user.name =>
    	    			"invalid"
    		}
    	)
  }

	// take stuff from queue
	def sleep : Boolean = {
		try {
			queue.take() match {
				case UserValidationRequest(_user: User, _actor: ActorRef, _validation: String) =>
					log("received validation request for " + _user)
					_actor ! (validate(_user))
					!Thread.currentThread().isInterrupted
				case ShutdownMessage(_msg: String, _time: Long) =>
					log(_msg)
					false
			}
		}
		catch {
			case e:InterruptedException =>
				log(s"Interrupted: ${e.getMessage}")
				false
		}
	}

	// sleep
  def run () = {
    log("starting")
    while (sleep) {}
		log("ended")
  }
}