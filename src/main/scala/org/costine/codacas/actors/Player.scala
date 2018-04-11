package org.costine.codacas.actors

import akka.actor._
import java.net._
import java.io._

import org.costine.codacas.network.Handler
import fi.utu.cs.physics.Body
import org.costine.codacas.{Naming, Torp}
import java.util.Date

import org.costine.codacas.database._
import org.costine.codacas.messages.BroadcastMessage
import org.costine.codacas.messages.CollisionMessage
import org.costine.codacas.messages.CommandMessage
import org.costine.codacas.messages.ExplodedMessage
import org.costine.codacas.messages.GameCharacterMessage
import org.costine.codacas.messages.GameShutdownMessage
import org.costine.codacas.messages.GetWarpMagnitudeMessage
import org.costine.codacas.messages.PlayerOnMessage
import org.costine.codacas.messages.PlayerRegistrationMessage
import org.costine.codacas.messages.SetWarpMagnitudeMessage
import org.costine.codacas.messages.ShutdownMessage
import org.costine.codacas.messages.TextMessage
import org.costine.codacas.messages.UserMessage
import org.costine.codacas.messages.WhoMessage
import org.costine.codacas.system.Logging

import scala.concurrent.Await


/**
 * Player actor is connected to a handler. 
 */
class Player(val userValidationService: UserValidationService)
  extends Actor with Naming with Logging {

  // various special characters
  val BEL: Short = 7
  val DEL: Short = 127
  val BS: Short = 8
  val ENTER: Short = 13
  val RETURN: Short = 10

	def debug = false

	var handlerMaybe:Option[Handler] = None
	
	var userMaybe:Option[User] = None

	/**
	 * the associated game actor reference.
	 */
	var gameActor : Option[ActorRef] = None

	/**
	 * set the time this Player was initialized
	 */
	lazy val startTime = new Date
 
	def id: String = handlerMaybe.map { _.id } getOrElse "undefined "
 
	var shipNameMaybe:Option[String] = None

	// this setting should survive between ship usages, and is set by the game during startup
  var wCommandMagnitudeFactor : Double = 0.001

	private val commandBuffer = new StringBuilder()
 
	private val commandChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ;:\"'[]{}+=-_()!@#$%^&*<>,.?/\\~`|"

	// when the buffer is empty these characters will be used to immediately 
	// get a result without typing enter. If the buffer is not empty, hitting 
	// enter clears the buffer and executes a multicharacter command
	private val singleKeyCommands = "GSYF?VR"

	def logPrefix = s"Player $id; ship ${shipNameMaybe getOrElse "unAttached"}"
 
	// send output to writer
 	def output (msg:String): Unit = handlerMaybe foreach { _.out(msg)}

	def outputln (msg:String): Unit = output(s"$msg\n\r")
  
 	/**  schedule a user validation to occur asyncronously */
 	def scheduleUserValidation (user:User): Unit =
    userValidationService.scheduleValidation(user, self)

  /** process a single character that is sent from the game */
  def processCharacterFromGame(char: Char) : Unit = {
    // ----
    // enter (13) or return (10) received --- send what's in this commandBuffer back to the Game
    def sendCommandBufferToGame(): Unit = {
			if (commandBuffer.nonEmpty) {
				output(char.toString)
				val cmd = commandBuffer.toString
        log(s"Player ${shipNameMaybe.getOrElse("Unknown")}, sending command '$cmd' to Game")
				commandBuffer.clear
				sender ! CommandMessage(cmd)
			}
      else log(s"Player ${shipNameMaybe.getOrElse("Unknown")}, not sending empty buffer to Game")
		}

    // ----
    // DEL/BS delete the previous character.
    // if there is nothing in the buffer and we BS/DEL (8/127), then ring the BEL (7)
    //
    def deleteCharacterFromCommandBuffer(): Unit = {
      val cbString = commandBuffer.toString
      if (cbString.nonEmpty) {
        output(char.toString)
        commandBuffer.clear
        commandBuffer.append(cbString.substring(0, cbString.length - 1))
      }
      else
        output(BEL.toChar.toString)
    }

    def sendSingleCharacterCommand(uc: Char): Unit = {
      outputln(char.toString)
      log(s"Player ${shipNameMaybe.getOrElse("Unknown")}, sending single char command '$uc' to Game")
      sender ! CommandMessage(uc.toString)
    }

    def addCharacterToCommandBuffer(): Unit = {
      commandBuffer.append(char.toString)
      output(char.toString)
    }

    // ----
    // convert to uppercase, and if it is a single character command
    // and the buffer is empty send it to the game for execution
    //
    def processCharacterFromCommandBuffer(): Unit = {
      val upperChar = char.toUpper
      singleKeyCommands.toCharArray
        .find(_ == upperChar)
        .fold {
          addCharacterToCommandBuffer()
        } { c => if (commandBuffer.isEmpty) sendSingleCharacterCommand(c) else addCharacterToCommandBuffer() }
    }

    val shortChar = char.toShort

    log(s"received '$char' ($shortChar) character from Game")

    shortChar match {
      case ENTER => sendCommandBufferToGame()
      case RETURN => sendCommandBufferToGame()
      case DEL => deleteCharacterFromCommandBuffer()
      case BS => deleteCharacterFromCommandBuffer()
      case _ => processCharacterFromCommandBuffer()
    }
  }

	def receive: PartialFunction[Any, Unit] = {

	    // ----
	    // initial message from the Game actor, establishing this Player
	    //
	  	case PlayerRegistrationMessage(handler, ship, cmf) =>

	  	  startTime
				gameActor = Some(sender)
	  	  handlerMaybe = Some(handler)
	  	  shipNameMaybe = Some(ship)
				wCommandMagnitudeFactor = cmf

				sender ! PlayerOnMessage(this)  // send this player back to the game actor to let it know we are set up
	  	  log("is on.")
	  	  outputln(s"You are ship $ship.")
	  	  sender ! BroadcastMessage(s"$ship is on.")


	  	// ----
	  	// A physics Body collided with the player associated ship.
	  	// Forward it up to the Game
	  	//
	  	case CollisionMessage(body:Body) =>
	  	  log("collision event received, forward to game")
				gameActor foreach { _ ! CollisionMessage(body) }

	  	// ----
	  	// a torp exploded somewhere and caused damage
	  	// 
	  	case ExplodedMessage(body:Torp, damage:Int) =>
	  	  log("torp explosion event received, forward to game")

				outputln {
          val shipName = body.ship.name
          shipNameMaybe.map { ship =>
            if (shipName != ship) s"Exploded torp from $shipName caused you $damage fpls damage"
            else s"Your own torp caused you $damage fpls damage"
          } getOrElse s"A torp from $shipName exploded with $damage, but you don't have a ship"
        }

	  	// ----
	  	// who am i
	  	//
	  	case _ : WhoMessage =>
				outputln (userMaybe.map {x => s"logged in as ${x.name}"} getOrElse "not logged in")

			case SetWarpMagnitudeMessage(mf: Double) =>
				wCommandMagnitudeFactor = mf
				outputln(s"warp magnitude factor set to $wCommandMagnitudeFactor")

			case _ : GetWarpMagnitudeMessage =>
				outputln(s"warp magnitude factor is $wCommandMagnitudeFactor")

	  	// ----
	  	// response from login validation
	  	// 
	  	case UserValidationResponse(user:User, valid:String) =>
	  	  val msg = valid match {
	  	    case "valid" =>
	  	      userMaybe = Some(user)
	  	      s"user ${user.name} logged in"
	  	    case "invalid" => s"user ${user.name} login failed"
          case x => s"user ${user.name} login failed; validation response message: $x"
	  	  }
        log(msg)
        outputln(msg)
	  	  
	  	// ----
	  	// got a character from the Game, so manipulate the command buffer
	  	// 
      case GameCharacterMessage(char:Char) => processCharacterFromGame(char: Char)

      // ----
      // send a non-blocking message to this player's output device
      //
      case TextMessage(text) =>
				log("received msg")
				outputln(text)
        
      	// ----
      	// someone wants to change this player's user
      	// 
      	// schedule a validation
      	//
			case UserMessage(user) =>
				log(s"received login request for $user")
				scheduleUserValidation (user)
      	  
      	// ----
        // received a ShutdownMessage, dump it, and stop this player (no act() invoked)
      	//
			case msg:ShutdownMessage =>
				log(s"received player shutdown msg ${msg.text}")

			// ----
			// received a GameShutdownMessage, dump it, and stop this players handler
			//
			case GameShutdownMessage(text,time) =>
				log(s"received player Game shutdown msg $text")
				outputln(s"shutting down: $text")
				import scala.concurrent._
				import ExecutionContext.Implicits.global
				handlerMaybe map { h => Future {h.socket.close()}}

	  	// ----
	  	// indicate (on the console) that an unhandled message was received
	  	//
	    case msg =>
				log(s"unhandled message: $msg")
				log(s"unhandled message class: ${msg.getClass.getName}")
	}

	def longMessage = s"$shortMessage${shipNameMaybe.fold("")(s => s"; ship $s")}"

	def shortMessage = s"player ${this.id}"
}

object Player {
	def ref(validationService: UserValidationService): ActorRef = actorSystem.actorOf(Props(new Player(validationService)))
}