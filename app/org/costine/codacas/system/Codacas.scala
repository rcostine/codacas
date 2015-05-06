package org.costine.codacas.system

import java.util.Date

import akka.actor.ActorRef
import org.costine.codacas.network._
import org.costine.codacas.actors._
import org.costine.codacas.database.UserValidationService
import play.api.Application
import scala.language.postfixOps

/**
 * Main class for Codacas game server
 * 
 * Starts the Game actor, which then waits for messages from:
 * the Reactor, TimeKeeper, and RechargeTimer threads.
 * 
 * Invoked like:
 *    -port [TCP-port]
 *    -interval [universe_update_interval]
 *    -debug [true|false]
 *    -recharge [recharge interval]
 *
 * via standalone main, or from Play environment via the "run" method
 *
 */
object Codacas {

  case class RunnableThread (runnable: Runnable, thread: Thread)

  trait CanShutdown {
    def shutdown : Unit
  }

  case class CodacasRuntime (gameActor : ActorRef, threads: List[RunnableThread])
    extends CanShutdown with Logging {

    def debug = false

    def logPrefix = "Codacas Runtime"

    def interruptAllLive = (threads filter { t => t.thread.isAlive } map { tt =>
      if (!tt.thread.isInterrupted) {
        tt.thread.interrupt()
      }
    })

    def shutdown : Unit = {
       gameActor ! GameShutdownMessage("codacas shutdown initiated", new Date().getTime)

       // interrupt all the threads and sleep for a second
       while (interruptAllLive.length > 0) {
         val v = threads filter { t => t.thread.isAlive } map { _.runnable.getClass.getName}
         log(s"Waiting for ${v.mkString(",")} threads to exit.")
         Thread.sleep(1000)
       }

       log("Shutting down codacas actor context")
       actorSystem.shutdown
     }
  }

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

    val result = run(args.toList)

    // finally send the CodacasRuntime to the game actor so the shutdown command works.
    result.gameActor ! result

  }


  def runViaPlay(app: Application) = {
    val root = "codacas"

    // create the parameter list
    val param = List(
      app.configuration.getBoolean(s"${root}.debug") map { p=>s"-debug ${p}"},
      app.configuration.getLong(s"${root}.interval") map { p=>s"-interval ${p}"},
      app.configuration.getInt(s"${root}.port") map { p=>s"-port ${p}"},
      app.configuration.getLong(s"${root}.recharge") map { p=>s"-recharge ${p}"}
    ) map {_.getOrElse("")} filter( q => q.trim.length > 0)

    // run the subsystem with the plist
    run(param.mkString(" ").split("  *").toList)

  }

  // shutdown
  def shutdown(rt : CodacasRuntime) = {
    rt.shutdown
  }

  // this will return the game actor and a list of threads
  // it should be called by the Global.scala during the Play startup lifecycle
  def run(lArgs: List[String]) = {
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
    val rThreads = List (
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
    ) map { instance => RunnableThread(instance,new Thread(instance))}

    // start the threads
    rThreads foreach { rt => rt.thread.start()}

    // return a game actor reference and a list of instances and threads
    // all of the threads are things that *will* block, and *cannot* be actors
    CodacasRuntime(g, rThreads :+ RunnableThread(userValidationService,userValidationServiceThread))
  }


}
