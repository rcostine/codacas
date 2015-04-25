package org.costine.codacas.system

import org.costine.codacas.network._
import org.costine.codacas.actors._
import org.costine.codacas.database.UserValidationService
import scala.language.postfixOps

/**
 * Main class for Codacas game server
 * 
 * Starts the Game actor, which then waits for messages from:
 * the Reactor, TimeKeeper, and RechargeTimer threads.
 * 
 * Invoked like: [TCP-port] [universe_update_interval]
 * 
 */
object Codacas {

  def parameter (p:String) : String = s"-${p}"

  def property[T](args: List[String],p:String, f: String => T) : Option[T] = {
    args match {
      case p :: value :: rest =>  Some(f(value))
      case Nil => None
      case head :: tail => property(tail,p,f)
    }
  }
  def debugProperty(args: List[String], p:String = parameter("debug")): Option[Boolean] = {
    property(args,p, {s => s.toBoolean})
  }
  def timeIntervalProperty(args: List[String], p:String = parameter("interval")): Option[Long] = {
    property(args,p, {s => s.toLong})
  }
  def portProperty(args: List[String], p:String = parameter("port")): Option[Int] = {
    property(args,p, {s => s.toInt})
  }
  def rechargeIntervalProperty(args: List[String], p:String = parameter("recharge")): Option[Long] = {
    property(args,p, {s => s.toLong})
  }

  def main(args : Array[String]) : Unit = {

    val lArgs = args.toList

    // ----
    // start the user validation service
    //
    val userValidationService = new UserValidationService
    val userValidationServiceThread = new Thread(userValidationService)
    userValidationServiceThread.start
    
    // ---
    // new game actor. This contains the game state, and acts as
    // a supervisor object. It contains the universe, and a list of
    // active players.
    //
    val g = Game.ref(userValidationService,debugProperty(lArgs) getOrElse(false))

    // start this list of threads, they can block, and will feed messages into the game actor when
    // various events occur.
    List (
      // ----
      // A Reactor generates Acceptor objects, which are then selected
      // on. For each accepted connection create a Handler
      // instance that is sent to the Game actor.
      // When the Game actor receives the Handler, it will create
      // a new Player object that is attached to a free ship.
      //
      // Methods in the Handler objects allow the Game to write to
      // the underlying socket connections.
      //
      new Reactor(portProperty(lArgs) getOrElse(2001),g),

      // ----
      // When the Game actor sees this object in its mailbox, it knows to update the universe.
      //
      new TimeKeeper(timeIntervalProperty(lArgs) getOrElse(100l),g),

      // ----
      // When the Game actor sees this object in its mailbox, it knows to invoke recharge on all universe objects
      // (just use 1 sec for now)
      //
      new RechargeTimer(rechargeIntervalProperty(lArgs) getOrElse(1000),g)
    ) foreach { thread => new Thread(thread).start }
  }
}
