package org.costine.codacas.database

import akka.actor.ActorRef
import java.util.concurrent.LinkedBlockingQueue

import org.costine.codacas.messages.ShutdownMessage
import org.costine.codacas.system.Logging

import scala.util.{Failure, Success, Try}

class UserValidationService extends Runnable with Logging {

	def logPrefix = "UserValidationService"

  def debug = false

	private val queue:LinkedBlockingQueue[Object] = new LinkedBlockingQueue

  def scheduleValidation (user:User, actor:ActorRef): Unit =
		queue.put(UserValidationRequest(user,actor,"unchecked"))

  def validate (user:User) = UserValidationResponse(
    user,
    // TODO database lookup to see if user is valid
    user.name match {
      case "rcostine" => "valid"
      case user.name => "invalid"
    }
  )

	// take stuff from queue
	def sleep : Boolean =
    Try {
      queue.take() match {
        case UserValidationRequest(user: User, actor: ActorRef, validation: String) =>
          log("received validation request for " + user)
          actor ! validate(user)
          !Thread.currentThread().isInterrupted
        case ShutdownMessage(msg: String, time: Long) =>
          log(msg)
          false
      }
    } match {
      case Success(b) => b
      case Failure(e: InterruptedException) =>
        log(s"Interrupted: ${e.getMessage}")
        false
      case Failure(t) =>
        log(s"validation failed because of an exception",t)
        false
    }


	// sleep
  def run (): Unit = {
    log("starting")
    while (sleep) {}
		log("ended")
  }
}