package org.costine.codacas.actors

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import java.util.Date
import java.net._
import java.io._
import org.costine.codacas.system._
import org.costine.codacas.network.Handler
import org.costine.codacas.network.ShutdownMessage
import org.costine.codacas.network.SocketClosedMessage
import org.costine.codacas.renderers._
import org.costine.codacas.interactions._
import fi.utu.cs.physics.Vector2
import fi.utu.cs.physics.Body
import org.costine.codacas._
import org.costine.codacas.database.{UserValidationService, User}
import java.lang.Math.abs

class Game(_debug:Boolean, _userValidationService: UserValidationService) extends Actor {

	def this(_debug:Boolean) = {
		this(_debug,null)
	}

	private var userValidationService: UserValidationService = _userValidationService
  
	private var uni:Universe =  newUniverse
 
	private var playerMap = scala.collection.mutable.HashMap.empty[ActorRef,Player]
 
	private var timeLastAdvance = new Date ()
 
	private var shuttingDown = false
 
	private val digits = "0123456789"
	private val dirs = "UDLR"
 
	private var dbg = _debug
 
	def debug = { dbg }
 
	val id = new Date ().getTime.toString

	def log (_x:String) = {
		println ("Game " + id + ": " + _x)
	}
 	
 	def debug (_x:String):Unit = {
 	  if (debug) {
 		  log (_x)
 	  }
 	}
 
	def universe (_u:Universe) = {
	  uni = _u
	}
 
	def universe:Universe = {
	  if (uni == null) {
		  newUniverse
	  }
	  uni
	}
 
	def newUniverse = {
	 
		val uv = new Universe(
				   200, 	// width 
				   200, 	// height
				   new UniverseParameters,
	               new StarFactory(.02, 
	                  1, 3,
	                  1000, 2000,
	                  50, 4000
				   ), // how the universe populates stars
	               5,						// stargate size (the size of this universe in the parent)
	               new Coordinate (0,0), 	// stargate location (loc of universe in parent)
	               null						// parent universe 
		)
	 
		// (re)-populate all of the stars 
		uv.repopulateStars
  
		uv
	}


  // Render Galactic scan to String
	def cmdGalacticScan():String = new CharacterArrayRenderer (this.universe, 20, " ", "\n\r").toString

  // Render short scan around player to String
	def cmdShortScan(_player:Player): String = {
    if (_player != null && _player.ship != null)
      Option(this.universe.shipByName(_player.ship)) map
        { _s => s"${new ShortRangeCharacterArrayRenderer(_s, 40, 11, " ", "\n\r").toString}\n\r${_s}" } getOrElse("")
    else ""
	}

  // show foople display
	def cmdFooples(_player:Player):String = {
		Option(this.universe.shipByName(_player.ship)) map { _s =>
      s"FPL=${_s.fooples}; WQL=${_s.wql}; T=${_s.torps.intValue}"
    } getOrElse ""
	}

  // set the velocity of a vector in the universe
  // Component is UDLR
	def setVelocity (_component:Char, _qty:String, _vect:Vector2) : Unit = {
		val a = (if (_qty.trim.length == 0) 1 else Integer.parseInt(_qty.trim)) / 1000.0
    _component match {
      case 'U' =>  _vect.setDeltaX(-a)
      case 'D' =>  _vect.setDeltaX(a)
      case 'L' =>  _vect.setDeltaY(-a)
      case 'R' =>  _vect.setDeltaY(a)
      case anything => log(s"invalid vector component encountered: ${anything}")
    }
	}

	// player wants to setup torps, or fire a torp in a direction
	// set torp parameters:
    // TRunits - set max torp range after explosion.
    // TFms - set max torp time (fuel). Torp explodes after ttl expires
    // TPfpls - torp payload in fooples
    // TSspeed - how fast a fired torp will initially move
    // T? - show current torp settings
    // TG - show small torp angle graph
    // 
    // T[angle] - fire a torp at the angle specified
	def cmdTorp (_opt:String, _player:Player, act: ActorRef) = {
	  val _shipName = _player.ship
	  val _ship = this.universe.shipByName(_player.ship)

    _opt.trim.toCharArray.toList match {
      case 'R' :: rest =>
        resetTorpRange(rest.mkString(""),_ship,act)
      case 'F' :: rest =>
        resetTorpFuel(rest.mkString(""),_ship,act)
      case 'P' :: rest =>
        resetTorpPayload(rest.mkString(""),_ship,act)
      case 'S' :: rest =>
        resetTorpSpeed(rest.mkString(""),_ship,act)
      case '?' :: rest =>
        act ! ("msg", _ship.torpInfoMessage("Torp Info: ", ";"))
      case 'G' :: rest =>
        val _tah = new HelpTorpAngles
        _tah.ship = _player.ship
        _tah.send(act)
      case anything =>
        fireTorp(anything.mkString(""),_ship,act)
    }
	}

  def transformNumber[T](s: String)(implicit tc: String => T, tt: T => T): Either[Throwable,Option[T]]  = {
    try {
      Right(if (s.length > 0) Some(tt(tc(s))) else None)
    }
    catch {
      case _ex:Exception =>
        Left(_ex)
    }
  }

  // reset (or just show) the torp range value
  def resetTorpRange(_v: String, _ship : Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = abs(v)

    val x = transformNumber (_v)(p,t) match {
      case Right(vv) => vv match {
        case Some(x) =>
          _ship.defaultTorpRange = x
          ("msg", s"new torp range: ${_ship.defaultTorpRange}")
        case None => ("msg", s" current torp range: ${_ship.defaultTorpRange}")
      }
      case Left(e) => ("msg", "invalid torp range entered")
    }
    act ! x
  }

  // reset (or just show) the torp fuel parameter
  def resetTorpFuel(_v:String, _ship:Ship, act: ActorRef) : Unit = {
    def p(v:String) : java.lang.Long = java.lang.Long.parseLong(v)
    def t(v:java.lang.Long) : java.lang.Long = abs(v)

    val x = transformNumber (_v)(p,t) match {
      case Right(vv) => vv match {
        case Some(x) =>
          _ship.defaultTorpFuel = x
          ("msg", s"new torp fuel (msec): ${_ship.defaultTorpFuel}")
        case None => ("msg",s" current torp fuel: ${_ship.defaultTorpFuel}")

      }
      case Left(e) => ("msg", "invalid torp fuel (msec) entered")
    }
    act ! x
  }

  // reset or show torp payload (amount that the torp will decrease opponent if it hits a ship)
  def resetTorpPayload(_v: String, _ship : Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Long = java.lang.Long.parseLong(v)
    def t(v:java.lang.Long) : java.lang.Long = abs(v)

    val x = transformNumber (_v)(p,t) match {
      case Right(vv) => vv match {
        case Some(x) =>
          _ship.defaultTorpFuel = x
          ("msg", s"new torp payload (fooples): ${_ship.defaultTorpPayload}")
        case None => ("msg",s" current torp payload: ${_ship.defaultTorpPayload}")

      }
      case Left(e) => ("msg", "invalid torp payload (fooples) entered")
    }
    act ! x
  }

  // reset or show torp payload (amount that the torp will decrease opponent if it hits a ship)
  def resetTorpSpeed(_v: String, _ship : Ship, act : ActorRef) : Unit = {

    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = abs(v)

    val x = transformNumber (_v)(p,t) match {
      case Right(vv) => vv match {
        case Some(x) =>
          _ship.defaultTorpSpeed = x
          ("msg", s"new torp speed (units): ${_ship.defaultTorpSpeed}")
        case None => ("msg",s" current torp speed: ${_ship.defaultTorpSpeed}")

      }
      case Left(e) => ("msg", "invalid torp speed (units) entered")
    }
    act ! x

  }

  // nnn means fire a torp in a direction, blank means fire in the current direction
  // anyting else is unknown
  def fireTorp(_opt: String, _ship: Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = v

    val x = transformNumber (_opt)(p,t) match {
      case Right(vv) => vv match {
        case Some(x) => ("msg", _ship.fireTorp(x).message)
        case None => ("msg",_ship.fireTorp.message)
      }
      case Left(e) => ("msg", s"unknown torp command: ${_opt}")
    }
    act ! x
  }

  // send a text message
	def cmdMsg (_opt:String, _fromPlayer:Player, act: ActorRef) = {
		if (_opt.length > 2) {
			val _to = _opt.substring(0,1).toUpperCase
      Option(universe.shipByName (_to)) map { _toShip =>
        val _fromShip = _fromPlayer.ship
        val _m = _opt.substring(1,_opt.length)
        getPlayer(_toShip.player) foreach { tp =>
          tp._1 ! ("msg", s"From ${_fromShip}: ${_m}")
          act ! ("msg", s"Message sent to ${_to}")
        }
      } getOrElse {
        act ! ("msg", s"${_to} is not on. Message not sent.")
      }
		}
	}

  // send a message to the player to ask who they are
	def cmdWho (act: ActorRef) = {
	  act ! ("who")
	}

  // send the message to login a player
	def cmdLogin (_login:String, act: ActorRef) = {
	  act ! (getUser(_login))
	}

  // get a user
	def getUser(_user:String) = {
	  val tokens = _user.split("  *")
	  User(tokens(0),tokens(1))
	}

  // message a player
  def msgPlayer (a: ActorRef, s: String) : Unit = {
    getPlayer(a) foreach { player =>
      player._1 ! ("msg", s)
    }
  }

  // Do phaserish things
	def cmdPhaser (_opt:String, _fromShip:Ship) = {
		val _player = _fromShip.player
		if (_opt.length > 2) {
			val _to = _opt.substring(0,1).toUpperCase
			val _toShip = universe.shipByName (_to)
			if (_toShip != null) {
			  if (_toShip.isAlive) {
				 if (!_toShip.yanked) {
					 try { 
						 val amt:Int = java.lang.Math.abs(Integer.parseInt(_opt.substring(1,_opt.length)))
						 val _phaser = new Phaser (_fromShip,_toShip,amt)
					 	 val _result = _phaser.interact
						 if (_result.code == _phaser.PHASER_OK) {
							val _pResult = _result.asInstanceOf[PhaserResult]
							if (_pResult.phaser.effective > 0) {

								getPlayer(_toShip.player) foreach { toPlayer =>
									toPlayer._1 !("msg", s"Phaser Hit of ${_pResult.phaser.effective}")
									if (_toShip.isDead) {
										toPlayer._1 ! ("msg","You are dead.")
										self ! ("broadcastAll", s"${_toShip.name} is dead.")
									}
								}
                msgPlayer(_fromShip.player,s"${_result.message}; effective hit: ${_pResult.phaser.effective}")
							}
							else {
                msgPlayer(_toShip.player,s"Ship ${_fromShip.name} tried to phaser you")
								msgPlayer(_fromShip.player,s"Phasers ineffective at this range on ship ${_toShip.name}")
							}
						 }
						 else msgPlayer(_fromShip.player,_result.message)

					 }
					 catch {
				 		case _ex:Exception =>
              _ex.printStackTrace(System.out)
              msgPlayer(_player, s"Usage: P${_toShip.name}<number>")
					 }
				 }
				 else msgPlayer(_player,s"${_to} is yanked")

			  }
			  else msgPlayer(_player,s"${_to} is already dead")
			}
			else {
				if (universe.validShipName(_to))
          msgPlayer(_player,s"Player ${_to} is not logged on")
			}
		}
		else msgPlayer(_player, "Usage: P<ship><fooples>")

	}
 
	// parse the whacky W command eg: wU1D3
	def cmdMove (_dir:String, _player:Player, act: ActorRef) = {
	  val _d = _dir.trim

	  val usingShip = this.universe.shipByName(_player.ship)

	  if (_d == "") {
	    usingShip.setVelocity(0,0)
	    act ! ("msg", "ship stopped")
	  }
	  else {


      // TODO: this is fugly and needs to be reworked
		  val _v = usingShip.getVelocity
		  var _cdr = ' '
		  var _qty:StringBuilder = new StringBuilder

		  for (_ddd <- _d.toArray) {

		    if (dirs.indexOf(_ddd.toString) > -1) {

		    	if (_cdr == ' ') {
		    	  _cdr = _ddd
		    	}
		    	else { 
		    		setVelocity(_cdr,_qty.toString,_v)
		    		_cdr = _ddd
		    		_qty = new StringBuilder
		    	}
		    }
		    else if (digits.indexOf(_ddd.toString) > -1) {
		    	if (_cdr != ' ') _qty.append(_ddd.toString)
		    }
		  }

		  if (_cdr != ' ') setVelocity(_cdr,_qty.toString,_v)
    
		  usingShip.setVelocity(_v)
		  usingShip.setAcceleration(0,0)
	  } 
	}

	// inform player that it is now off
	def playerOff (act: ActorRef, _player:Player) = {

		// remove the ship from the universe
		universe.shipOff(_player.ship)

		// remove the player from the active player map
		playerMap -= act
    	
		// tell everyone player "x" is off, don't bother telling player though
		playerMap.foreach(p =>
      	     p._1 ! ("msg", s"${_player.ship} is off")
		)
		log(s"player ${_player.id}; ship ${_player.ship} is off")
		log(s"${playerMap.size} players left.")
     
		// this will end the player actor
		act ! (new ShutdownMessage("socket closed", 0))

	}

	// get the player tuple using a player
	def getPlayer (_refPlayer:Player) = {
	   for (
			 (act,_player) <- playerMap
			   if _player.id.equals(_refPlayer.id)
	   ) yield (act,_player)
	}

	// get the player tuple using the actor ref
	def getPlayer (_refActor:ActorRef) = {
		for (
			(act,_player) <- playerMap
			if act.equals(_refActor)
		) yield (act,_player)
	}

	// get the player tuple using a handler
	def getPlayer (_handler:Handler) = {
		for (
			(act,_player) <- playerMap
			if _player.id.equals(_handler.id)
		) yield (act,_player)
	}


	// restart the player's dead ship
	def restart (_player:Player, act: ActorRef) = {
		var _s = universe.shipByName(_player.ship)
		if (_s.isDead) {
			if (_s != null) {
				_s.restart
				universe.placeShip(_s)
				act ! ("msg", "Restarted.")
				self ! ("broadcastAll", s"${_s.name} restarted.")
			}
		}
		else {
			act ! ("msg", "Restart not possible. You are not dead yet.")
		}
	}


	// register a new player
	def newPlayer (h: Handler) = {
		val s = this.universe.newShip
		if (s != null) {
			val act = Player.ref(this.userValidationService)
			s.player(act)
			act ! (h, s.name)
		}
		else {
			h.out(new ShutdownMessage("game full. try later.", 10000))
		}
	}

	/**
	 * Handle messages directed to us from other actors or objects.
	 */
	def receive = {

		// ----
		// A Handler as message, with nothing else, means that a new Player has joined the game
		// Allocate a Ship (if all ships in use. send a Shutdown message and close the socket after 10 secs)
		// Make a new Player actor.
		// Add the Player actor reference to the ship.
		// Start the Player actor
		// Send the Player actor a message with this Game, the handler, and the ship name (A-Z). This last step activates the Player actor.
		//
		case (_handler: Handler) =>
			newPlayer(_handler)

		case (p: Player) =>
			log(s"game registering player ${p.longMessage}")
			playerMap += (sender -> p)

		// ---
		// Handler has indicated that it closed the underlying socket.
		// This means we need to sign the player off.
		//
		case (_msg: SocketClosedMessage) =>
			log("socket closed. player is being removed")
			val players = getPlayer(sender)

			players foreach { p => playerOff(p._1, p._2) }


		// ----
		// forward a character from a handler back to a player
		//
		//
		case (_char: Char, _handler: Handler) =>
			val players = getPlayer(_handler)
			log("received a character from handler " + _handler.id)
			players foreach {
				p => {
					if (_char.toShort == 4) {
						// ctrl-D
						_handler.close
						playerOff(p._1, p._2)
					}
					else {
            log(s"sending char ${_char} to player ${p._2.id}")
            p._1 ! (_char, "game")
          }
				}
			}

		// ----
		// Something we can name smacked into the ship associated with player
		//
		case ("collide", _body: Naming, _player: Player) =>
			val players = getPlayer(_player)
			players foreach { p => {
          log(p._2.thisCollidingMessage(_body))
          p._1 !("msg", p._2.collidingMessage(_body))
        }
			}


		// ----
		// broadcast message (from a player) to all players except the sending player
		//
		case ("broadcast", _text: String) =>
			getPlayer(sender) foreach { _sender =>
				log(s"received broadcast: ${_text} from ship ${_sender._2.ship}; id = ${_sender._2.id}")

				for {
					_p <- playerMap if _p._2.id != _sender._2.id
				} _p._1 ! ("msg", _text)
			}

    // ----
    // broadcast message from the game to all players
    //
    case ("broadcastAll", _text: String) =>
      log(s"received broadcastAll from Game, broadcast to all players")
      playerMap foreach { p =>  p._1 ! ("msg", _text) }

		// ----
		// message from a timer indicating that we should advance the universe
		//
		case (_timer: TimeKeeper) =>
			debug("universe advance message from timer")
			val newDate = new Date()
			val interval = newDate.getTime - timeLastAdvance.getTime
			timeLastAdvance = newDate
			this.universe.timeMarchesOn(interval.toDouble)

		// ----
		// message from a timer indicating that we should recharge objects
		//
		case (_timer: RechargeTimer) =>
			debug("universe recharge message from timer")
			this.universe.recharge

		// ----
		// "cmd" message from a player indicating that we should do something with
		// the universe and then send a text message back to the player
		//
		// Anything that messes with the underlying universe needs to message this Game
		// in order for everything to be orderly and synchronized.
		//
		// Players are not allowed to mess with the universe directly.
		//
		case ("cmd",_cmd: String) =>

      log(s"Game received command: ${_cmd}")

			getPlayer(sender) foreach { p =>
				val act = p._1
				val _player = p._2

				log("command " + _cmd + " from player " + _player.id + "; Ship " + _player.ship)
				val _ship = universe.shipByName(_player.ship)


				// yank the player from the game
				if (_cmd != "Y" && _ship.unyank) {
					act ! ("msg", "unyanked")
					self ! ("broadcastAll", s"${_player.ship} unyanked.")
				}

				// galactic scan
				if (_cmd == "G") {
					act ! ("msg", cmdGalacticScan)
				}

				// short scan
				else if (_cmd == "S") {
					act ! ("msg", cmdShortScan(_player))
				}

				// show fooples
				else if (_cmd == "F") {
					act ! ("msg", cmdFooples(_player))
				}

				// show help
				else if (_cmd == "?") {
					new HelpInfo().send(act)
				}

				// show version
				else if (_cmd == "V") {
					new Info().send(act)
				}

				// restart
				else if (_cmd == "R") {
					restart(_player,act)
				}

				// yank
				else if (_cmd == "Y") {
					if (universe.shipByName(_player.ship).yank) {
						act ! ("msg", "yanked")
						self ! ("broadcastAll",s"${_player.ship} yanked.")
					}
					else {
						act ! ("msg", "already yanked")
					}
				}
				else {
					val _cmdLine = _cmd.split("  *")
					val _cmdtrim = _cmd.trim
					var _exec = ""
					var _execUc = ""
					var _param = ""
					if (_cmdLine.length > 0) {
						_exec = _cmdLine(0)
						_execUc = _exec.toUpperCase
						_param = _cmd.substring(_exec.length, _cmd.length)
					}
					// Who is the player logged in as
					if (_cmdtrim.toUpperCase.equals("WHO")) {
						cmdWho(act)
					}
					// move the ship
					else if (_execUc.startsWith("W") && _ship.isAlive) {

						if (_execUc.length > 1) {
							cmdMove(_execUc.substring(1, _execUc.length), _player, act)
						}
						else {
							cmdMove("", _player, act)
						}
					}
					// phaser someone
					else if (_execUc.startsWith("P") && _ship.isAlive) {
						if (_execUc.length > 1) {
							cmdPhaser(_execUc.substring(1, _execUc.length), _ship)
						}
					}
					// torp someone
					else if (_execUc.startsWith("T") && _ship.isAlive) {
						if (_execUc.length > 1) {
							cmdTorp(_execUc.substring(1, _execUc.length), _player, act)
						}
					}
					// message someone
					else if (_cmdtrim.toUpperCase.startsWith("M")) {
						if (_cmdtrim.length > 1) {
							cmdMsg(_cmdtrim.substring(1, _cmdtrim.length), _player,act)
						}
					}
					// login the player
					else if (_cmdtrim.toUpperCase.startsWith("LOGIN ")) {
						if (_cmdtrim.length > 6) {
							cmdLogin(_cmdtrim.substring(6, _cmdtrim.length),act)
						}
					}
					else if (_ship.isDead) {
						act ! ("msg", "You are still dead.")
					}
					else {
						act ! ("msg", s"unknown command: ${_cmd}")
					}
				}
	  }
    case (cm: SocketClosedMessage, _handler: Handler) =>
      getPlayer(_handler) foreach { p => playerOff (p._1,p._2) }

    case ("collide", b: Body) =>
      getPlayer(sender) foreach { p =>
        if (b.isInstanceOf[Star]) {
          val star = b.asInstanceOf[Star]
          p._1 ! ("msg", s"Collision with star of radius ${star.getRadius}.")
          cmdMove("",p._2,p._1)
        }
      }

		case msg =>
			log(s"unhandled message: ${msg}")
      log(s"unhandled message class: ${msg.getClass.getTypeName}")
	}
}

object Game {
	def ref : ActorRef = ref(new Game(false))

	def ref(userService: UserValidationService, debug: Boolean): ActorRef =  {
		actorSystem.actorOf(Props(new Game(debug)))
	}

	def ref(g: Game):ActorRef = ActorDSL.actor(g)
}
