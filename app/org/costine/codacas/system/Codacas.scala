package org.costine.codacas.system

import org.costine.codacas.network._
import org.costine.codacas.actors._
import org.costine.codacas.database.UserValidationService

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
  def main(args : Array[String]) : Unit = {
    
    // ----
    // start the user validation service
    //
    val userValidationService = new UserValidationService
    val userValidationServiceThread = new Thread(userValidationService)
    userValidationServiceThread.start
    
    // ---
    // new game actor. This contains the game state, and acts as
    // a controller object. It contains the universe, and a list of
    // active players.
    //
    val g = Game.ref(userValidationService,false)
    
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
    val c = new Reactor(Integer.parseInt(args(0)),g)
  	val network = new Thread(c)
    network.start

    // ----
    // When the Game actor sees this object in its mailbox, it knows to update the universe.
    //
  	val t = new TimeKeeper(java.lang.Long.parseLong(args(1)),g)
  	val timer = new Thread(t)
    timer.start
    
    // ----
    // When the Game actor sees this object in its mailbox, it knows to invoke recharge on all universe objects
    // (just use 1 sec for now)
    //
    val r = new RechargeTimer(1000,g)
  	val recharger= new Thread(r)
    recharger.start
  }
}
