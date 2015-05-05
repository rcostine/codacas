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

	// Directions for the "W" command
	private val digits = "0123456789"
	private val decimals = s"${digits}."
	private val dirs = "UDLR"

	case object Directions extends Directions

  trait Directions {

    sealed trait Component

    def parse (s: String): Either[String,List[Component]] = parse(s.toList)(Nil)

    def parse (s: List[Char])(c: List[Component]): Either[String,List[Component]] = {
			s match {
				case x :: rest =>
					if (digits.indexOf(x) > -1) {
						val (cc,more) = findFirstNotIn(rest,digits)
						parse (more)(c :+ Magnitude(value = (x :: cc).mkString.toInt))
					}
					else {
						val d = getDirection(x)
						if (d.isDefined)
							parse(rest)(c :+ d.get)
						else
							scala.util.Left(s"Invalid direction component: ${x}")
					}
				case Nil =>
					scala.util.Right(c)
			}
    }


    case class Magnitude (value: Int) extends Component

    sealed trait Dir {
      def value: Int
    }

    case object Positive extends Dir {
      val value: Int = 1
    }
    case object Negative extends Dir {
      val value: Int = -1
    }

    trait Direction extends Component {
			def value: Char
			def dir: Dir
		}

    case class HDirection(value: Char, dir: Dir ) extends Direction

    case class VDirection(value: Char, dir: Dir) extends Direction


    val directions : List[Direction] = List(
			VDirection ('U', Negative),
			VDirection ('D', Positive),
			HDirection ('L', Negative),
			HDirection ('R', Positive)
		)

    // add upper and lowercase directions to translating Map
    lazy val directionsByChar: Map[Char,Direction] =
      directions.foldLeft(Map[Char,Direction]()){(b,a) =>
                        {
													val v = b.+((a.value,a)).+((a.value.toUpper,a))
													v
												}
      }

    def getDirection(c: Char) = {
			log(directionsByChar.mkString)
			val v = directionsByChar.get(c.toUpper)
			log(v.toString)
			v
		}

		// parse each of the components
		// the magnitude factor is used to control how fast WQLs are used up
		// normal value is .001, but it can be set to any number.
		// .01 will use up WQLs ten times faster.
		def parseComponents(l: List[Component],mf: Double)(vect: Vector2): Vector2 = {

			val n = copy(vect)
			l match {
					// W2L4U
				case Magnitude(m1) :: HDirection(v1,d1) :: Magnitude(m2) :: VDirection(v2,d2) :: rest =>
					n.setDeltaY(n.getDeltaY+(m1*mf*d1.value))
					n.setDeltaX(n.getDeltaX+(m2*mf*d2.value))
					parseComponents(rest,mf)(n)
				// Wl2U4
				case HDirection(v1,d1) :: Magnitude(m1) :: VDirection(v2,d2) :: Magnitude(m2) :: rest =>
					n.setDeltaY(n.getDeltaY+(m1*mf*d1.value))
					n.setDeltaX(n.getDeltaX+(m2*mf*d2.value))
					parseComponents(rest,mf)(n)
				// W2U4L
				case Magnitude(m1) :: VDirection(v1,d1) :: Magnitude(m2) :: HDirection(v2,d2) :: rest =>
					n.setDeltaY(n.getDeltaY+(m2*mf*d2.value))
					n.setDeltaX(n.getDeltaX+(m1*mf*d1.value))
					parseComponents(rest,mf)(n)
				// W1U
				case Magnitude(m) :: VDirection(v,d) :: rest =>
					n.setDeltaX(n.getDeltaX+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// WU1
				case VDirection(v,d) :: Magnitude(m) :: rest =>
					n.setDeltaX(n.getDeltaX+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// W1L
				case Magnitude(m) :: HDirection(v,d) :: rest =>
					n.setDeltaY(n.getDeltaY+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// WL1
				case HDirection(v,d) :: Magnitude(m) :: rest =>
					n.setDeltaY(n.getDeltaY+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// WUL1
				case VDirection(v1,d1) :: HDirection(v,d) :: Magnitude(m) :: rest =>
					n.setDeltaX(n.getDeltaX+(m*mf*d1.value))
					n.setDeltaY(n.getDeltaY+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// WLU1
				case HDirection(v,d) :: VDirection(v1,d1) :: Magnitude(m) :: rest =>
					n.setDeltaX(n.getDeltaX+(m*mf*d1.value))
					n.setDeltaY(n.getDeltaY+(m*mf*d.value))
					parseComponents(rest,mf)(n)
				// WUL1
				case VDirection(v1,d1) :: HDirection(v,d) :: rest =>
					n.setDeltaX(n.getDeltaX+(mf*d1.value))
					n.setDeltaY(n.getDeltaY+(mf*d.value))
					parseComponents(rest,mf)(n)
				// WLU1
				case HDirection(v,d) :: VDirection(v1,d1) :: rest =>
					n.setDeltaX(n.getDeltaX+(mf*d1.value))
					n.setDeltaY(n.getDeltaY+(mf*d.value))
					parseComponents(rest,mf)(n)
				// WL
				case HDirection(v,d) :: rest =>
					n.setDeltaY(n.getDeltaY+(mf*d.value))
					parseComponents(rest,mf)(n)
				// WU
				case VDirection(v1,d1) :: rest =>
					n.setDeltaX(n.getDeltaX+(mf*d1.value))
					parseComponents(rest,mf)(n)
				case _ => vect
			}
		}

		def copy(v: Vector2) : Vector2 = {
			new Vector2(v.getDeltaX,v.getDeltaY)
		}

  }


	private var userValidationService: UserValidationService = _userValidationService
  
	private var uni:Universe =  newUniverse
 
	private var playerMap = scala.collection.mutable.HashMap.empty[ActorRef,Player]
 
	private var timeLastAdvance = new Date ()
 
	private var shuttingDown = false

	private lazy val validShipNames = universe.validShipNames.mkString.toCharArray
 
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
	  if (uni == null) newUniverse
	  uni
	}

	//  makes a new universe
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

  // Render short scan around player to String ("S" defaults to the 40 units around player
	def cmdShortScan(_player:Player): String = {
		cmdShortScan(_player,40)
	}

	// Render Ultra Short scan around player to String
	def cmdShortScan(_player:Player, value: Double): String = {
		if (_player != null && _player.ship != null)
			Option(this.universe.shipByName(_player.ship)) map
				{ _s => s"${new ShortRangeCharacterArrayRenderer(_s, value, 11, " ", "\n\r").toString}\n\r${_s}" } getOrElse("")
		else ""
	}

  // show foople display
	def cmdFooples(_player:Player):String = {
		Option(this.universe.shipByName(_player.ship)) map { _s =>
      s"FPL=${_s.fooples}; WQL=${_s.wql}; T=${_s.torps.intValue}"
    } getOrElse ""
	}

	// returns all the characters up to the one that is not in the list in the
	// first element, and the rest (or Nil) in the second one
	def findFirstNotIn (m: List[Char], valid: String) : (List[Char], List[Char]) = {
		// get List of found/notfound
		val v = m map { n => valid.indexOf(n)}
		val pos = v.indexOf(-1)  // find first one not found
		if (pos == -1) (m,Nil) else m.splitAt(pos)
	}

	def plotCourse (from:UniverseObject, to: UniverseObject, speed: Double): Vector2 = {
		val v = new Vector2(from.getPosition, to.getPosition)
		val l = v.length() * speed
		v.setDeltaX(v.getDeltaX * l)
		v.setDeltaY(v.getDeltaY * l)
		v
	}



	def gotoPlayer(_fromPlayer : Player, _toPlayer: Option[Player], speed: Double) : Option[Vector2] = {
		_toPlayer map { _tp =>
			val _from = this.universe.shipByName(_fromPlayer.ship)
			val _to = this.universe.shipByName(_tp.ship)
			val vect = plotCourse(_from, _to, speed * _fromPlayer.wCommandMagnitudeFactor)
			_from.setVelocity(vect)
			vect
		}
	}

	def gotoStar(_fromPlayer : Player, _toStar: Option[Star], speed: Double) : Option[Vector2] = {
		_toStar map { _tp =>
			val _from = this.universe.shipByName(_fromPlayer.ship)
			val vect = plotCourse(_from, _tp, speed * _fromPlayer.wCommandMagnitudeFactor)
			_from.setVelocity(vect)
			vect
		}
	}

	def gotoClosestStar (_fromPlayer: Player, speed: Double) : Vector2 = {
		val _from = this.universe.shipByName(_fromPlayer.ship)
		val (_to,_todist) = findNearestStar(_from)
		val vect = plotCourse(_from,_to, speed * _fromPlayer.wCommandMagnitudeFactor)
		_from.setVelocity(vect)
		// we are moving, therefore no longer colliding with star
		if (!_from.stopped) _from.collidedWithStar = None

		_from.setAcceleration(0,0)
		vect
	}

	def gotoNearestShip(_fromPlayer : Player, speed: Double) : Vector2 = {
		val _from = this.universe.shipByName(_fromPlayer.ship)
		val (_to,_todist) = findNearestShip(_from)
		val vect = plotCourse(_from,_to, speed * _fromPlayer.wCommandMagnitudeFactor)
		_from.setVelocity(vect)
		vect
	}

	// gets nearest ship that is not us
	def findNearestShip (_fromShip: Ship) : (Ship, Double) = {
		universe.ships filter { fs => !fs.name.equals(_fromShip.name)} map {s => (s,_fromShip.distance(s))} minBy {_._2}
	}

	// gets nearest star
	def findNearestStar (_fromShip: Ship) : (Star, Double) = {
		universe.stars map {s => (s,_fromShip.distance(s))} minBy {_._2}
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

    val l =  _opt.trim.toCharArray.toList match {
      case x :: y => x.toUpper :: y
      case Nil => Nil
    }

    l match {
      case 'R' :: rest =>
				val (tp, more) = findFirstNotIn(rest,digits)
        resetTorpRange(tp.mkString,_ship,act)
				doCmd(more)(act,_player,_ship)
      case 'F' :: rest =>
				val (tp, more) = findFirstNotIn(rest,digits)
        resetTorpFuel(tp.mkString,_ship,act)
				doCmd(more)(act,_player,_ship)
      case 'P' :: rest =>
				val (tp, more) = findFirstNotIn(rest,digits)
        resetTorpPayload(tp.mkString,_ship,act)
				doCmd(more)(act,_player,_ship)
      case 'S' :: rest =>
				val (tp, more) = findFirstNotIn(rest,digits)
        resetTorpSpeed(rest.mkString,_ship,act)
				doCmd(more)(act,_player,_ship)
      case '?' :: rest =>
        msgPlayer(act,_ship.torpInfoMessage("Torp Info: ", ";"))
				doCmd(rest)(act,_player,_ship)
      case 'G' :: rest =>
        val _tah = new HelpTorpAngles
        _tah.ship = _player.ship
        _tah.send(act)
				doCmd(rest)(act,_player,_ship)
      case anything =>
				val (tp, more) = findFirstNotIn(anything,digits)
        if (!sendMsgIfDead(act, _ship, Some((List('T') :: tp).mkString.toList))) {
          fireTorp(tp.mkString, _ship, act)
        }
				doCmd(more)(act,_player,_ship)
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
        case Some(x) => _ship.fireTorp(x).message
        case None => _ship.fireTorp.message
      }
      case Left(e) => s"unknown torp command: ${_opt}"
    }
    msgPlayer(act,x)
  }

  // send a text message -- this always eats up the rest of the line
	// even if there is no ship
	def cmdMsg (_opt:String, _fromPlayer:Player, act: ActorRef) = {
		if (_opt.length > 2) {
			val _to = _opt.substring(0,1).toUpperCase
      Option(universe.shipByName (_to)) match {
        case Some(_toShip) =>
          val _fromShip = _fromPlayer.ship
          val _m = _opt.substring (1, _opt.length)
          getPlayer (_toShip.player) foreach ( tp => {
            msgPlayer (tp._1, s"From ${_fromShip}: ${_m}")
            msgPlayer (act, s"Message sent to ${_to}")
          })
        case None => msgPlayer(act, s"${_to} is not on. Message not sent.")
      }
		}
	}

  // send a message to the player to ask who they are
	def cmdWho (act: ActorRef) : Unit = {
	  act ! ("who")
	}

  // send the message to login a player
	def cmdLogin (_login:String, act: ActorRef) = {
	  act ! (getUser(_login))
	}

	// special commands start with ":" and are for future expansion.
	def cmdSpecial (m: List[Char],_fromPlayer:Player, act: ActorRef) = {
		val s = m.mkString.trim
		if (s.toUpperCase.startsWith("LOGIN "))
			cmdLogin(s.substring("LOGIN ".length, s.length),act)
		else if (s.toUpperCase.equals("WARP MAGNITUDE"))
			cmdWarpMagnitude("",act)
		else if (s.toUpperCase.startsWith("WARP MAGNITUDE "))
			cmdWarpMagnitude(s.substring("WARP MAGNITUDE ".length,s.length).trim,act)
		else
			msgPlayer(act,s"Unknown Special Command: ${s}")
	}

	// set the scale of the warp magnitude components
	def cmdWarpMagnitude(s:String,act:ActorRef) = {
		if (s.trim.length == 0)
			act ! ("warp", "magnitude")
		else
			try
				act ! ("warp", "magnitude", s.toDouble)
			catch {
				case t : Throwable => msgPlayer(act, s"Cannot set warp magnitude ${s}; ${t.getMessage}")
			}
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

  // Do phaserish thing to the toShip - like a 'PA100'
	def cmdPhaser (amt: Int, _toShip: Ship)(act: ActorRef, _player:Player, _ship: Ship) : Unit= {
		if (_toShip.isAlive) {
		 if (!_toShip.yanked) {
			 val _phaser = new Phaser (_ship,_toShip,amt)
			 val _result = _phaser.interact
			 if (_result.code == _phaser.PHASER_OK) {
				val _pResult = _result.asInstanceOf[PhaserResult]
				if (_pResult.phaser.effective > 0) {

					getPlayer(_toShip.player) foreach { toPlayer =>
						msgPlayer(toPlayer._1,s"Phaser Hit of ${_pResult.phaser.effective}")
						if (_toShip.isDead) {
							msgPlayer(toPlayer._1,"You are dead.")
							self ! ("broadcastAll", s"${_toShip.name} is dead.")
						}
					}
					msgPlayer(act,s"${_result.message}; effective hit: ${_pResult.phaser.effective}")
				}
				else {
					msgPlayer(_toShip.player,s"Ship ${_ship.name} tried to phaser you")
					msgPlayer(act,s"Phasers ineffective at this range on ship ${_toShip.name}")
				}
			 }
			 else msgPlayer(act,_result.message)
		 }
		 else msgPlayer(act,s"${_toShip.name} is yanked")
		}
		else msgPlayer(act,s"${_toShip.name} is already dead")
	}

  // parse the whacky W command eg: wU1D3
  // Use cases:
  // Nil: stop ship
  //
  // [dir]n[dir]n
  // n[hdir][hdir]...
  //
	def cmdMove (m: List[Char])(act: ActorRef, _player:Player, _ship: Ship) : Unit = {
		val _d = m.mkString.trim.toUpperCase

	  val usingShip = this.universe.shipByName(_player.ship)

	  if (_d == "") {
	    usingShip.setVelocity(0,0)
	    msgPlayer(act,"ship stopped")
	  }
	  else {
			val components = Directions.parse(m)(Nil)
			components match {
				case Left(msg) => msgPlayer(act,msg)
				case Right(l) =>
					val vel = Directions.parseComponents(l,_player.wCommandMagnitudeFactor)(Vector2.ZERO)
					usingShip.setVelocity(vel)

					// we are moving, therefore no longer colliding with star
					if (!usingShip.stopped) _ship.collidedWithStar = None

					usingShip.setAcceleration(0,0)
			}
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
	// send back the handler, the ship name, and the initial value for warp command magnitude factor
	def newPlayer (h: Handler) = {
		val s = this.universe.newShip
		if (s != null) {
			val act = Player.ref(this.userValidationService)
			s.player(act)
			act ! (h, s.name, this.universe.wCommandMagnitudeFactor)
		}
		else {
			h.out(new ShutdownMessage("game full. try later.", 10000))
		}
	}

	// toggle player yank state
	def cmdYank (_player : Player, _ship: Ship, act : ActorRef) : Boolean = {

		if (_ship.unyank) {
			act ! ("msg", "unyanked")
			self ! ("broadcastAll", s"${_player.ship} unyanked.")
			true
		}
		else {
			if (_ship.yank) {
				act ! ("msg", "yanked")
				self ! ("broadcastAll", s"${_player.ship} yanked.")
				false
			}
			else {
				act ! ("msg", "already yanked")
				false
			}
		}
	}


	// short command version of sendMsgIfDead
	def sendMsgIfDead(act: ActorRef, _ship: Ship, cmd : Char) : Boolean = {
		  sendMsgIfDead(act,_ship,Some(List(cmd)))
	}

	// send the player a message if they try to do a command that is not allowed when dead.
	def sendMsgIfDead(act: ActorRef, _ship: Ship, cmd : Option[List[Char]]) : Boolean = {
		if (_ship.isDead) {
			cmd foreach {c => msgPlayer(act, s"Command '${cmd.mkString}' is not operational because you are still dead.") }
		}
		_ship.isDead()
	}

	def stillDead(act : ActorRef, _ship: Ship): Unit= {
		if (_ship.isDead()) {
			act !("msg", s"BTW, you are still dead.")
		}
	}

	// do a command for an actor, player, ship
	def doCmd(m : List[Char])(act: ActorRef , _player : Player, _ship : Ship) : Unit = {

		m match {
			case n :: rr =>
				val l = n.toUpper :: rr
				l match {

					// Toggle yanking
					case 'Y' :: rest =>
						cmdYank(_player, _ship, act)
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Galactic scan
					case 'G' :: rest =>
						act ! ("msg", cmdGalacticScan)
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Short Scan
					case 'S' :: rest =>
						msgPlayer(act,cmdShortScan(_player))
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Fooples display
					case 'F' :: rest =>
						act ! ("msg", cmdFooples(_player))
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Help
					case '?' :: rest =>
						new HelpInfo ().send (act)
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// About and dedications
					case 'V' :: rest =>
						new Info ().send (act)
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Restart
					case 'R' :: rest =>
						restart (_player, act)
						doCmd (rest)(act,_player,_ship)

					// move (warp) or "WHO" command
					case 'W' :: rest =>
						// Who
						if (rest.mkString.toUpperCase.startsWith("HO")) {
							cmdWho(act)
							stillDead(act,_ship)
							doCmd(rest.drop(2))(act,_player,_ship)
						}
						// Move (Wxxxxx)
						else {
							val (c, more) = findFirstNotIn(rest, s"${dirs}${dirs.toLowerCase}${digits}")
							if (!sendMsgIfDead(act,_ship,Some(c))) {
								cmdMove(c)(act, _player, _ship)
							}
							doCmd(more)(act,_player,_ship)
						}
					case 'U' :: rest =>
						val (c, more) = findFirstNotIn(rest, s"${decimals}")
						if (c == Nil) {
							msgPlayer(act,cmdShortScan(_player,4.0))
						}
						else {
							try {
								val d = c.mkString.toDouble
								msgPlayer(act, cmdShortScan(_player, d))
							}
							catch {
								case e: Throwable => msgPlayer(act,s"Ultra short scan failed (try 4 to get a really short scan): ${e.getMessage}")
							}
						}
						stillDead(act,_ship)
						doCmd(more)(act,_player,_ship)
					// phaser  P<A-Z>nnnnn
					case 'P' :: rest =>
						rest match {

							// p should be a ship name A-Z
							case p :: pRest =>
								validShipNames.find(z => z == p.toUpper) match {

                  case None =>
                    msgPlayer(act, s"Usage: P<ship><fooples>; Cannot phaser Ship '${p}'. It is not a valid ship name. Ships are addressed A-Z.")

                  case Some(pp) =>
                    Option[Ship](universe.shipByName(pp.toUpper.toString)) match {
                      case None =>
                        msgPlayer(act, s"Ship ${p} is not on.")
                        val (c, more) = findFirstNotIn(pRest, s"${digits}")
                        doCmd(more)(act, _player, _ship)

                      case Some(toShip) =>
                        val (c, more) = findFirstNotIn(pRest, s"${digits}")
                        if (c.size > 0) {
                          // P<player> with some amount
                          if (!sendMsgIfDead(act, _ship, Some(s"P${p}${c.mkString}".toList)))
                            cmdPhaser(c.mkString.toInt, toShip)(act, _player, _ship)
                        }
                        else msgPlayer(act,
                          if (_ship.isAlive()) s"Usage: P<ship><fooples>; Phasering ${toShip} requires entering fooples <= ${_ship.fooples}"
                          else s"You are still dead.")

                        doCmd(more)(act, _player, _ship)
                    }
                }

							// just a "P" entered
							case Nil =>
								msgPlayer(act, "Usage: P<ship><fooples>")

						}

          // message a player
          case 'T' :: rest =>
            cmdTorp(rest.mkString, _player,act)

					// message a player
					case 'M' :: rest =>
						cmdMsg(rest.mkString, _player,act)

					// move towards closest star
					case 'C' :: rest =>
						val (c, more) = findFirstNotIn(rest, s"${decimals}")
						if (c == Nil) {
							gotoClosestStar(_player,1)
							msgPlayer(act,cmdShortScan(_player))
						}
						else {
							try {
								val d = c.mkString.toDouble
								gotoClosestStar(_player,d)
								msgPlayer(act, cmdShortScan(_player))
							}
							catch {
								case e: Throwable => msgPlayer(act,s"Closest star nav failed: ${e.getMessage}")
							}
						}
						stillDead(act,_ship)
						doCmd(more)(act,_player,_ship)

					// do a "special command" -- they always end a line
					case ':' :: rest =>
						cmdSpecial(rest,_player,act)

					// otherwise it is unknown
					case cc :: rest =>
						msgPlayer(act,s"Unknown Command: ${cc}")
						doCmd(rest)(act,_player,_ship)

					// Anything but nil will allow this to recurse for the line.
          case Nil =>
            log("End of command, encountered")
				}
      case Nil =>
        log("End of command, encountered")
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
				val (act, _player) = p
				log("command " + _cmd + " from player " + _player.id + "; Ship " + _player.ship)
				doCmd(_cmd.trim.toList)(act,_player, universe.shipByName(_player.ship))
	  	}

    case (cm: SocketClosedMessage, _handler: Handler) =>
      getPlayer(_handler) foreach { p => playerOff (p._1,p._2) }

    case ("collide", b: Body) =>
      getPlayer(sender) foreach { p =>
        if (b.isInstanceOf[Star]) {
          val star = b.asInstanceOf[Star]
          p._1 ! ("msg", s"Collision with star of radius ${star.getRadius}.")
          val shp = universe.shipByName(p._2.ship)
          shp.collidedWithStar = Option(star)
          cmdMove(Nil)(p._1,p._2,shp)
        }
      }

		case msg =>
			log(s"unhandled message: ${msg}")
      log(s"unhandled message class: ${msg.getClass.getName}")
	}
}

object Game {
	def ref : ActorRef = ref(new Game(false))

	def ref(userService: UserValidationService, debug: Boolean): ActorRef =  {
		actorSystem.actorOf(Props(new Game(debug,userService)))
	}

	def ref(g: Game):ActorRef = ActorDSL.actor(g)
}
