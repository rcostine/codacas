package org.costine.codacas.database

import akka.actor.ActorRef
import java.util.concurrent.LinkedBlockingQueue

case class ShutdownMessage (_msg:String, _time:Long)

class UserValidationService extends Runnable {

  private val queue:LinkedBlockingQueue[Object] = new LinkedBlockingQueue
  
  
  def scheduleValidation (_user:User, _actor:ActorRef) = {
	queue.put(UserValidationRequest(_user,_actor,"unchecked"))
  }
  
  def log (_x:String) = {
		println ("UserValidationService: " + _x)
  }
  
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
  
  def run () = {
    log("starting")
    var done = false
    while (!done)
    	queue.take() match {
    		case UserValidationRequest (_user:User,_actor:ActorRef,_validation:String) =>
    		  log("received validation request for " + _user)
    		  _actor ! (validate(_user))
    		  
    		case ShutdownMessage (_msg:String, _time:Long) => 
    		  log(_msg)
    		  done=true
    	}
    
  }
}