package org.costine.codacas.actors

import akka.actor._
import java.net._
import java.io._
import org.costine.codacas.network.Handler
import org.costine.codacas.network.ShutdownMessage
import fi.utu.cs.physics.Body
import org.costine.codacas.{Naming, Torp}
import java.util.Date
import org.costine.codacas.database._


/**
 * Player actor is connected to a handler. 
 */
class Player(_userValidationService: UserValidationService) extends Actor with Naming {
	var handler:Handler = null
	
	var user:User = User(null,null)
	
	/**
	 * Use this as user validation service
	 */
	var userValidationService: UserValidationService = _userValidationService

	/**
	 * the associated game actor reference.
	 */
	var game : Option[ActorRef] = None

	/**
	 * set the time this Player was initialized
	 */
	lazy val startTime = new Date
 
	def id = {
	  	if (handler != null)
		  handler.id
		else {
			"undefined"
		}
	}
 
	var ship:String = null
	//var game:ActorRef = null

	// this setting should survive between ship usages, and is set by the game during startup
  var wCommandMagnitudeFactor : Double = 0.001


	private var commandBuffer = new StringBuilder()
 
	private val commandChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ;:\"'[]{}+=-_()!@#$%^&*<>,.?/\\~`|"

	// when the buffer is empty these characters will be used to immediately 
	// get a result without typing enter. If the buffer is not empty, hitting 
	// enter clears the buffer and executes a multicharacter command
	private val singleKeyCommands = "GSYF?VR"
 
 	def log (_x:String) = {
		println ("Player " + id + "; ship " + ship + " : " + _x)
	}
 
	// send output to writer
 	def output (_x:String) = {
		handler.out(_x)
	}
  
	def outputln (_x:String) = {
		output(_x + "\n\r")
	}

  // deal with messages from the game
	/*
 	private def fromGame (_cmd:String, _g:ActorRef, act: ActorRef):Boolean = {
 	  if (game == _g) {
 		  true
	  }
 	  else {
 		  log("command \"" + _cmd + "\" received from unauthorized game")
 		  act ! ("refused", _cmd, self)
			true
 	  } 
 	}
 	*/
  
 	/**
 	 * schedule a user validation to occur asyncronously
 	 */
 	def scheduleUserValidation (_user:User) = {
 	  if (userValidationService != null) {
 	    userValidationService.scheduleValidation(_user, self)
 	  }
 	}
 	
	def receive = {

	    // ----
	    // initial message from the Game actor, establishing this Player
	    //
	  	case (_handler:Handler, _ship:String, _wCommandMagnitudeFactor: Double) =>

	  	  startTime
				game = Some(sender)
	  	  handler = _handler
	  	  ship = _ship
				wCommandMagnitudeFactor = _wCommandMagnitudeFactor

				sender ! (this)  // send this player back to the game actor to let it know we are set up
	  	  log("is on.")
	  	  outputln(s"You are ship ${_ship}.")
	  	  sender ! ("broadcast", s"${_ship} is on.")


	  	// ----
	  	// A physics Body collided with the player associated ship.
	  	// Forward it up to the Game
	  	//
	  	case ("collide", _body:Body) =>
	  	  log("collision event received, forward to game")
				game foreach { g => g ! ("collide",_body) }


	  	// ----
	  	// a torp exploded somewhere and caused damage
	  	// 
	  	case ("exploded", _body:Torp, _damage:Int) =>
	  	  log("torp explosion event received, forward to game")
				val _s = _body.ship.name
				outputln (
						if (_s != this.ship)
							s"Exploded torp from ${_s} caused you ${_damage} fpls damage"
						else
							s"Your own torp caused you ${_damage} fpls damage"
				)

	  	// ----
	  	// who am i
	  	//
	  	case ("who") =>  
	  	  val name = user.name
				outputln (
					if (name != null) s"logged in as $name" else "not logged in"
				)

			case("warp", "magnitude", mf: Double) => {
				wCommandMagnitudeFactor = mf
				outputln(s"warp magnitude factor set to ${wCommandMagnitudeFactor}")
			}

			case("warp", "magnitude") => {
				outputln(s"warp magnitude factor is ${wCommandMagnitudeFactor}")
			}


	  	// ----
	  	// response from login validation
	  	// 
	  	case (UserValidationResponse(_user:User, _valid:String)) =>
	  	  _valid match {
	  	    case "valid" =>
	  	      user = _user
	  	      val msg = s"user ${_user.name} logged in"
	  	      log(msg)
	  	      outputln(msg)
	  	      
	  	    case "invalid" =>
	  	      val msg = s"user ${_user.name} login failed"
	  	      log(msg)
	  	      outputln(msg)
	  	  }

	  	  
	  	// ----
	  	// got a character from the Game, need to append the character to the buffer
	  	// 
      case (_char:Char,"game") =>
      	  
				val shortChar = _char.toShort

				log("received character: " + shortChar)

				// ----
				// enter or return received --- send what's in this commandBuffer back to the Game
				if (shortChar == 13 || shortChar == 10) {
					if (commandBuffer.length > 0) {
						output(_char.toString)
						val _cmd = commandBuffer.toString
						commandBuffer.clear
						sender ! ("cmd", _cmd)
					}
				}

				// ----
				// DEL/BS delete the previous character
				//
				else if ((shortChar == 127) || (shortChar == 8)) {
					if (commandBuffer.length > 0) {
						output(_char.toString)
						val _cmd = commandBuffer.toString
						var _s =  commandBuffer.toString
						_s = _s.substring(0,_s.length - 1)
						commandBuffer.clear
						commandBuffer.append(_s)
					}
					// ----
					// DEL with nothing in buffer results in bell ring
					//
					else {
						val bel:Short = 7
						output(bel.toChar.toString)
					}
				}

				// ----
				// convert to uppercase, and if it is a single character command
				// and the buffer is empty send it to the game for execution
				//
				else {
					val _uc = _char.toString.toUpperCase
					val _sc = singleKeyCommands.indexOf(_uc)
					if ( _sc > -1) {
						if (commandBuffer.length == 0) {
							outputln(_char.toString)
							sender ! ("cmd", _uc)
						}
						else {
							commandBuffer.append(_char.toString)
							output(_char.toString)
						}
					}
					else {
					 commandBuffer.append(_char.toString)
					 output(_char.toString)
					}
				}


      	// ----
        // send a non-blocking message to this player's output device
      	//
      case ("msg",_text:String) =>
				log("received msg")
				outputln(_text)

        
      	// ----
      	// someone wants to change this player's user
      	// 
      	// schedule a validation
      	//
			case (_user:User) =>
				log(s"received login request for ${_user}")
				scheduleUserValidation (_user)
      	  
      	// ----
        // received a ShutdownMessage, dump it, and stop this player (no act() invoked)
      	//
			case (_msg:ShutdownMessage) =>
				log(s"received player shutdown msg ${_msg.text}")
      	
	  	// ----
	  	// indicate (on the console) that an unhandled message was received
	  	//
	    case msg =>
				log(s"unhandled message: ${msg}")
				log(s"unhandled message class: ${msg.getClass.getName}")
	}

	def longMessage = s"${shortMessage}${Option(ship).fold("")(s => s"; ship ${s}")}'"

	def shortMessage = s"player ${this.id}"
}

object Player {
	def ref(p: UserValidationService) = actorSystem.actorOf(Props(new Player(p)))
}