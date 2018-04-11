package org.costine.codacas.actors

import akka.actor._
import org.costine.codacas.system.Codacas.CodacasRuntime
import java.util.Date

import org.costine.codacas.system._
import org.costine.codacas.network.Handler
import org.costine.codacas.renderers._
import org.costine.codacas.interactions._
import fi.utu.cs.physics.Vector2
import fi.utu.cs.physics.Body
import org.costine.codacas._
import org.costine.codacas.database.{User, UserValidationService}
import java.lang.Math.abs

import org.costine.codacas.messages.BroadcastMessage
import org.costine.codacas.messages.BroadcastTextMessage
import org.costine.codacas.messages.CharacterFromHandlerMessage
import org.costine.codacas.messages.CollisionMessage
import org.costine.codacas.messages.CommandMessage
import org.costine.codacas.messages.GameCharacterMessage
import org.costine.codacas.messages.GameShutdownMessage
import org.costine.codacas.messages.GetWarpMagnitudeMessage
import org.costine.codacas.messages.NamedCollisionMessage
import org.costine.codacas.messages.PlayerHandlerRegistrationMessage
import org.costine.codacas.messages.PlayerOffMessage
import org.costine.codacas.messages.PlayerOnMessage
import org.costine.codacas.messages.PlayerRegistrationMessage
import org.costine.codacas.messages.SetWarpMagnitudeMessage
import org.costine.codacas.messages.ShutdownMessage
import org.costine.codacas.messages.SocketClosedMessage
import org.costine.codacas.messages.TextMessage
import org.costine.codacas.messages.UserMessage
import org.costine.codacas.messages.WhoMessage

import scala.collection.mutable
import scala.util.{Left, Right, Try}

class Game(_debug:Boolean, _userValidationService: Option[UserValidationService]) extends Actor with Logging {

	def this(_debug:Boolean) = {
		this(_debug,None)
	}

	val id: String = new Date ().getTime.toString

	def logPrefix = s"Game $id"

	// Directions for the "W" command
	private val digits = "0123456789"
	private val decimals = s"$digits."
	private val dirs = "UDLR"

	private var codacasRuntime: Option[CodacasRuntime] = None

	case object Directions extends Directions

  trait Directions {

    sealed trait Component

    def parse (s: String): Either[String,List[Component]] = parse(s.toList)(Nil)

    def parse (s: List[Char])(c: List[Component]): Either[String,List[Component]] = s match {
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
            Left(s"Invalid direction component: $x")
        }
      case Nil =>
        Right(c)
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

    def getDirection(c: Char): Option[Direction] = {
			log(directionsByChar.mkString)
			val v = directionsByChar.get(c.toUpper)
			log(v.toString)
			v
		}

		// parse each of the components
		// the magnitude factor is used to control how fast WQLs are used up
		// normal value is .001, but it can be set to any number.
		// .01 will use up WQLs ten times faster.
		def parseComponents(list: List[Component],mf: Double)(vect: Vector2): Vector2 = {

      def withNewVect(f : Vector2 => Unit) : Vector2 = {
        val nv = new Vector2(vect.getDeltaX,vect.getDeltaY)
        f(nv)
        nv
      }

			list match {
					// W2L4U
				case Magnitude(m1) :: HDirection(v1,d1) :: Magnitude(m2) :: VDirection(v2,d2) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaY(n.getDeltaY+(m1*mf*d1.value))
              n.setDeltaX(n.getDeltaX+(m2*mf*d2.value))
            }
          )
				// Wl2U4
				case HDirection(v1,d1) :: Magnitude(m1) :: VDirection(v2,d2) :: Magnitude(m2) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaY(n.getDeltaY+(m1*mf*d1.value))
              n.setDeltaX(n.getDeltaX+(m2*mf*d2.value))
            }
          )
				// W2U4L
				case Magnitude(m1) :: VDirection(v1,d1) :: Magnitude(m2) :: HDirection(v2,d2) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaY(n.getDeltaY+(m2*mf*d2.value))
              n.setDeltaX(n.getDeltaX+(m1*mf*d1.value))
            }
          )
				// W1U
				case Magnitude(m) :: VDirection(v,d) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaX(n.getDeltaX+(m*mf*d.value))}
          )
				// WU1
				case VDirection(v,d) :: Magnitude(m) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaX(n.getDeltaX+(m*mf*d.value))}
          )
				// W1L
				case Magnitude(m) :: HDirection(v,d) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaY(n.getDeltaY+(m*mf*d.value))}
          )
				// WL1
				case HDirection(v,d) :: Magnitude(m) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaY(n.getDeltaY+(m*mf*d.value))}
          )
				// WUL1
				case VDirection(v1,d1) :: HDirection(v,d) :: Magnitude(m) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaX(n.getDeltaX+(m*mf*d1.value))
              n.setDeltaY(n.getDeltaY+(m*mf*d.value))
            }
          )
				// WLU1
				case HDirection(v,d) :: VDirection(v1,d1) :: Magnitude(m) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaX(n.getDeltaX+(m*mf*d1.value))
              n.setDeltaY(n.getDeltaY+(m*mf*d.value))
            }
          )
				// WUL1
				case VDirection(v1,d1) :: HDirection(v,d) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaX(n.getDeltaX+(mf*d1.value))
              n.setDeltaY(n.getDeltaY+(mf*d.value))
            }
          )
				// WLU1
				case HDirection(v,d) :: VDirection(v1,d1) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n =>
              n.setDeltaX(n.getDeltaX+(mf*d1.value))
              n.setDeltaY(n.getDeltaY+(mf*d.value))
            }
          )
				// WL
				case HDirection(v,d) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaY(n.getDeltaY+(mf*d.value))}
          )
				// WU
				case VDirection(v1,d1) :: rest =>
					parseComponents(rest,mf)(
            withNewVect {n => n.setDeltaX(n.getDeltaX+(mf*d1.value))}
          )
				case _ => vect
			}
		}
  }


	private var userValidationService: Option[UserValidationService] = _userValidationService
  
	private var uni:Option[Universe] = Some(newUniverse)
 
	private val playerMap = scala.collection.mutable.HashMap.empty[ActorRef,Player]
 
	private var timeLastAdvance = new Date ()
 
	private var shuttingDown = false

	private lazy val validShipNames = universe.validShipNames.mkString.toCharArray
 
	private var dbg = _debug
 
	def debug: Boolean = dbg
 
	def universe (_u:Universe): Unit = uni = Some(_u)
 
	def universe:Universe = uni.getOrElse(newUniverse)

	//  makes a new universe
	def newUniverse: Universe = {
	 
		val uv = new Universe(
      200, 	// width
      200, 	// height
      new UniverseParameters,
      new StarFactory(.02, 1, 3, 1000, 2000, 50, 4000), // how the universe populates stars
      5,						// stargate size (the size of this universe in the parent)
      new Coordinate (0,0), 	// stargate location (loc of universe in parent)
      null						// parent universe
		)
	 
		// (re)-populate all of the stars 
		uv.repopulateStars()
  
		uv
	}


  // Render Galactic scan to String
	def cmdGalacticScan():String = new CharacterArrayRenderer (this.universe, 20, " ", "\n\r").toString

  // Render short scan around player to String ("S" defaults to the 40 units around player
	def cmdShortScan(_player:Player): String = cmdShortScan(_player,40)

	// Render Ultra Short scan around player to String
	def cmdShortScan(player:Player, value: Double): String = (for {
      shipName <- player.shipNameMaybe
      ship <- this.universe.shipByName(shipName)
      scan <- Some(s"${new ShortRangeCharacterArrayRenderer(ship, value, 11, " ", "\n\r").toString}\n\r$ship")
    } yield scan).getOrElse("")

  // show foople display
	def cmdFooples(player:Player):String = (for {
      shipName <- player.shipNameMaybe
      ship <- this.universe.shipByName(shipName)
      msg <- Some(s"FPL=${ship.fooples}; WQL=${ship.wql}; T=${ship.torps.intValue}")
    } yield msg).getOrElse("")

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

	def gotoPlayer(fromPlayer : Player, toPlayer: Option[Player], speed: Double) : Option[Vector2] = for {
    tp <- toPlayer
    toShipName <- tp.shipNameMaybe
    to <-  this.universe.shipByName(toShipName)
    fromShipName <- fromPlayer.shipNameMaybe
    from <- this.universe.shipByName(fromShipName)
    vect <- Some {
      val v = plotCourse(from, to, speed * fromPlayer.wCommandMagnitudeFactor)
      from.setVelocity(v)
      v
    }
  } yield vect

	def gotoStar(fromPlayer : Player, _toStar: Option[Star], speed: Double) : Option[Vector2] = for {
    tp <- _toStar
    fromShipName <- fromPlayer.shipNameMaybe
    from <- this.universe.shipByName(fromShipName)
    vect <- Some {
      val v = plotCourse(from, tp, speed * fromPlayer.wCommandMagnitudeFactor)
      from.setVelocity(v)
      v
    }
  } yield vect

	def gotoClosestStar (fromPlayer: Player, speed: Double) : Option[Vector2] =
    for {
      fromShipName <- fromPlayer.shipNameMaybe
      fromShip <- this.universe.shipByName(fromShipName)
      (to,todist) = findNearestStar(fromShip)
      vect = {
        val v = plotCourse(fromShip, to, speed * fromPlayer.wCommandMagnitudeFactor)
        fromShip.setVelocity(v)
        if (!fromShip.stopped) fromShip.collidedWithStar = None
        fromShip.setAcceleration(0,0)
        v
      }
    } yield vect

	def gotoNearestShip(fromPlayer : Player, speed: Double) : Option[Vector2] = for {
    fromShipName <- fromPlayer.shipNameMaybe
    fromShip <- this.universe.shipByName(fromShipName)
    (to,todist) = findNearestShip(fromShip)
    vect <- Some {
      val v = plotCourse(fromShip, to, speed * fromPlayer.wCommandMagnitudeFactor)
      fromShip.setVelocity(v)
      v
    }
  } yield vect

	// gets nearest ship that is not us
	def findNearestShip (fromShip: Ship) : (Ship, Double) =
    (universe.ships filter { fs => !fs.name.equals(fromShip.name)})
      .map {s => (s,fromShip.distance(s))} minBy {_._2}


	// gets nearest star
	def findNearestStar (_fromShip: Ship) : (Star, Double) =
		universe.stars map {s => (s,_fromShip.distance(s))} minBy {_._2}

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
	def cmdTorp (opt:String, player:Player, act: ActorRef) : Unit = {

    (for {
      shipName <- player.shipNameMaybe
      ship <- this.universe.shipByName(shipName)
    } yield ship).foreach { ship =>

      val l = opt.trim.toCharArray.toList match {
        case Nil => Nil
        case x :: y => x.toUpper :: y
      }

      l match {
        case 'R' :: rest =>
          val (tp, more) = findFirstNotIn(rest,digits)
          resetTorpRange(tp.mkString,ship,act)
          doCmd(more)(act,player,ship)
        case 'F' :: rest =>
          val (tp, more) = findFirstNotIn(rest,digits)
          resetTorpFuel(tp.mkString,ship,act)
          doCmd(more)(act,player,ship)
        case 'P' :: rest =>
          val (tp, more) = findFirstNotIn(rest,digits)
          resetTorpPayload(tp.mkString,ship,act)
          doCmd(more)(act,player,ship)
        case 'S' :: rest =>
          val (tp, more) = findFirstNotIn(rest,digits)
          resetTorpSpeed(rest.mkString,ship,act)
          doCmd(more)(act,player,ship)
        case '?' :: rest =>
          msgPlayer(act,ship.torpInfoMessage("Torp Info: ", ";"))
          doCmd(rest)(act,player,ship)
        case 'G' :: rest =>
          HelpTorpAngles(player.shipNameMaybe).send(act)
          doCmd(rest)(act,player,ship)
        case anything =>
          val (tp, more) = findFirstNotIn(anything,digits)
          if (!sendMsgIfDead(act, ship, Some((List('T') :: tp).mkString.toList))) {
            fireTorp(tp.mkString, ship, act)
          }
          doCmd(more)(act,player,ship)
      }
    }
	}

  def transformNumber[T](s: String)(implicit tc: String => T, tt: T => T): Either[Throwable,Option[T]] =
    Try {if (s.length > 0) Some(tt(tc(s))) else None }.toEither

  // reset (or just show) the torp range value
  def resetTorpRange(_v: String, ship : Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = abs(v)
    act ! {
      transformNumber(_v)(p, t) match {
        case Right(vv) => vv match {
          case Some(torpRange) =>
            ship.defaultTorpRange = torpRange
            TextMessage(s"new torp range: ${ship.defaultTorpRange}")
          case None => TextMessage(s" current torp range: ${ship.defaultTorpRange}")
        }
        case Left(e) => TextMessage("invalid torp range entered")
      }
    }
  }

  // reset (or just show) the torp fuel parameter
  def resetTorpFuel(_v:String, ship:Ship, act: ActorRef) : Unit = {
    def p(v:String) : java.lang.Long = java.lang.Long.parseLong(v)
    def t(v:java.lang.Long) : java.lang.Long = abs(v)

    act ! {
      transformNumber(_v)(p, t) match {
        case Right(vv) => vv match {
          case Some(x) =>
            ship.defaultTorpFuel = x
            TextMessage(s"new torp fuel (msec): ${ship.defaultTorpFuel}")
          case None => TextMessage(s" current torp fuel: ${ship.defaultTorpFuel}")

        }
        case Left(e) => TextMessage("invalid torp fuel (msec) entered")
      }
    }

  }

  // reset or show torp payload (amount that the torp will decrease opponent if it hits a ship)
  def resetTorpPayload(_v: String, ship : Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Long = java.lang.Long.parseLong(v)
    def t(v:java.lang.Long) : java.lang.Long = abs(v)

    act ! {
      transformNumber (_v)(p,t) match {
        case Right(vv) => vv match {
          case Some(x) =>
            ship.defaultTorpFuel = x
            TextMessage(s"new torp payload (fooples): ${ship.defaultTorpPayload}")
          case None => TextMessage(s" current torp payload: ${ship.defaultTorpPayload}")

        }
        case Left(e) => TextMessage("invalid torp payload (fooples) entered")
      }
    }
  }

  // reset or show torp payload (amount that the torp will decrease opponent if it hits a ship)
  def resetTorpSpeed(_v: String, ship : Ship, act : ActorRef) : Unit = {

    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = abs(v)

    act ! {
      transformNumber (_v)(p,t) match {
        case Right(vv) => vv match {
          case Some(ts) =>
            ship.defaultTorpSpeed = ts
            TextMessage(s"new torp speed (units): ${ship.defaultTorpSpeed}")
          case None =>
            TextMessage(s" current torp speed: ${ship.defaultTorpSpeed}")
        }
        case Left(e) => TextMessage("invalid torp speed (units) entered")
      }
    }

  }

  // nnn means fire a torp in a direction, blank means fire in the current direction
  // anyting else is unknown
  def fireTorp(opt: String, ship: Ship, act : ActorRef) : Unit = {
    def p(v:String) : java.lang.Double = java.lang.Double.parseDouble(v)
    def t(v:java.lang.Double) : java.lang.Double = v

    msgPlayer(act,

        transformNumber (opt)(p,t) match {
          case Right(vv) => vv match {
            case Some(x) => ship.fireTorp(x).message
            case None => ship.fireTorp.message
          }
          case Left(e) => s"unknown torp command: $opt"
        }

    )
  }

  // send a text message -- this always eats up the rest of the line
	// even if there is no ship
	def cmdMsg (opt:String, fromPlayer:Player, act: ActorRef): Unit = opt.toCharArray.toList match {
      case Nil =>  msgPlayer(act, s"Player not specified. Message not sent.")
      case toRaw :: Nil =>  msgPlayer(act, s"Message not specified. Nothing was sent to ${toRaw.toUpper}.")
      case toRaw :: rest =>
        val to = toRaw.toUpper.toString
        universe.shipByName(to) match {
          case Some(toShip) =>
            (for {
              fromShip <- fromPlayer.shipNameMaybe
              (toAct,toPlayer) <- getPlayer (toShip.player).headOption
              x <- Some {
                msgPlayer (toAct, s"From $fromShip: ${rest.mkString}")
                msgPlayer (act, s"Message sent to $to")
              }
            } yield x).getOrElse(())
          case None =>
            msgPlayer(act, s"$to is not on. Message not sent.")
        }
    }

  // send a message to the player to ask who they are
	def cmdWho (act: ActorRef) : Unit = {
	  act ! WhoMessage()
	}

  // send the message to login a player
	def cmdLogin (login:String, act: ActorRef): Unit =
    getUser(login).collect { case user: User => act ! UserMessage(user)}

	// special commands start with ":" and are for future expansion.
	def cmdSpecial (m: List[Char],_fromPlayer:Player, act: ActorRef): Unit = {

		val s = m.mkString.trim
    val sUpper = s.toUpperCase()
		if (sUpper.startsWith("LOGIN ")) cmdLogin(s.substring("LOGIN ".length, s.length),act)
		else if (sUpper.equals("WARP MAGNITUDE")) cmdWarpMagnitude("",act)
		else if (sUpper.startsWith("WARP MAGNITUDE ")) cmdWarpMagnitude(s.substring("WARP MAGNITUDE ".length,s.length).trim,act)
		else if (sUpper.trim.equals("SHUTDOWN")) codacasRuntime foreach { _.shutdown() }
		else msgPlayer(act,s"Unknown Special Command: $s")
	}

	// set the scale of the warp magnitude components
	def cmdWarpMagnitude(s:String,act:ActorRef): Unit =
		if (s.trim.isEmpty)
			act ! GetWarpMagnitudeMessage()
		else
      Try {act ! SetWarpMagnitudeMessage(s.toDouble)}.recover {
        case t => msgPlayer(act, s"Cannot set warp magnitude $s; ${t.getMessage}")
      }.getOrElse(())

  // get a user
	def getUser(creds:String): Option[User] = creds.split("  *").toList match {
    case Nil => None
    case user :: password :: _ => Some(User(user,password))
    case user :: Nil => None
  }

  // message a player
  def msgPlayer (a: ActorRef, s: String) : Unit =
    getPlayer(a) foreach { case (act,_) => act ! TextMessage(s) }


  // Do phaserish thing to the toShip - like a 'PA100'
	def cmdPhaser (amt: Int, toShip: Ship)(act: ActorRef, _player:Player, _ship: Ship) : Unit = {
    if (toShip.isAlive) {
      if (!toShip.yanked) {
        val _result = new Phaser(_ship, toShip, amt).interact()
        if (_result.code == Phaser.PHASER_OK) {
          _result match {
            case pResult: PhaserResult =>
              if (pResult.phaser.effective > 0) {

                getPlayer(toShip.player) foreach {
                  case (playerAct, toPlayer) =>
                    msgPlayer(playerAct, s"Phaser Hit of ${pResult.phaser.effective}")
                    if (toShip.isDead) {
                      msgPlayer(playerAct, "You are dead.")
                      self ! BroadcastTextMessage(s"${toShip.name} is dead.")
                    }
                }
                msgPlayer(act, s"${_result.message}; effective hit: ${pResult.phaser.effective}")

              }
              else {
                msgPlayer(toShip.player, s"Ship ${_ship.name} tried to phaser you")
                msgPlayer(act, s"Phasers ineffective at this range on ship ${toShip.name}")
              }
            case _ =>
              log(s"unexpected result from phaser interaction ${_result.code}; ${_result.message}")

          }
        }
        else msgPlayer(act, _result.message)
      }
      else msgPlayer(act, s"${toShip.name} is yanked")
    }
    else msgPlayer(act, s"${toShip.name} is already dead")
  }


  // parse the whacky W command eg: wU1D3
  // Use cases:
  // Nil: stop ship
  //
  // [dir]n[dir]n
  // n[hdir][hdir]...

  // msgPlayer(act,m)
  //
	def cmdMove (m: List[Char])(act: ActorRef, _player:Player, _ship: Ship) : Unit = {

    case class MoveMessage(msg: String) {
      def send(): Unit = msgPlayer(act,msg)
    }

    _player.shipNameMaybe.flatMap { shipName =>
      this.universe.shipByName(shipName).flatMap { usingShip =>

        if (m.mkString.trim.toUpperCase == "") {
          usingShip.setVelocity(0,0)
          Some(MoveMessage("ship stopped"))
        }
        else {
            Directions.parse(m)(Nil) match {
            case Left(msg) => Some(MoveMessage(msg))
            case Right(l) =>
              val vel = Directions.parseComponents(l,_player.wCommandMagnitudeFactor)(Vector2.ZERO)
              usingShip.setVelocity(vel)

              // we are moving, therefore no longer colliding with star
              if (!usingShip.stopped) _ship.collidedWithStar = None

              usingShip.setAcceleration(0,0)
              None
          }
        }
      }
    }.foreach(_.send())

	}

	// inform player that it is now off
	def playerOff (act: ActorRef, _player:Player): Unit = {

    log(_player.shipNameMaybe.map { ship =>
        // remove the ship from the universe
        universe.shipOff(ship)

        // remove the player from the active player map
        playerMap -= act

        // tell everyone player "x" is off, don't bother telling player though
        playerMap.foreach {case (p,_) => p ! TextMessage(s"$ship is off")}

        s"player ${_player.id}; ship $ship is off"
      }.getOrElse {s"Weird. Player leaving game has no ship name."}
    )
    log(s"${playerMap.size} players left.")
    // this will end the player actor
    act ! new ShutdownMessage("socket closed", 0)
	}

	// get the player tuple using a player
	def getPlayer (_refPlayer:Player): mutable.HashMap[ActorRef, Player] =
    playerMap.filter { case (_,_player) => _player.id.equals(_refPlayer.id)}

	// get the player tuple using the actor ref
	def getPlayer (_refActor:ActorRef): mutable.HashMap[ActorRef, Player] =
    playerMap.filter { case (act,_) => act.equals(_refActor) }

	// get the player tuple using a handler
	def getPlayer (_handler:Handler): mutable.HashMap[ActorRef, Player] =
    playerMap.filter { case (_,_player) => _player.id.equals(_handler.id)}


	// restart the player's dead ship
	def restart (player:Player, act: ActorRef): Unit =
    (for {
      shipName <- player.shipNameMaybe
      ship <- universe.shipByName(shipName)
    } yield ship).foreach {
      ship =>
        if (ship.isDead) {
          ship.restart()
          universe.placeShip(ship)
          act ! TextMessage("Restarted.")
          self ! BroadcastTextMessage(s"${ship.name} restarted.")
        }
        else
          act ! TextMessage("Restart not possible. You are not dead yet.")
    }

	// register a new player
	// send back the handler, the ship name, and the initial value for warp command magnitude factor
	def newPlayer (h: Handler): Unit = this.universe.newShip() match {
    case Some(ship) =>
      this.userValidationService match {
        case Some(uvService) =>
          val act = Player.ref(uvService)
          ship.player(act)
          act ! PlayerRegistrationMessage(h, ship.name, this.universe.wCommandMagnitudeFactor)
        case None =>
          h.out(ShutdownMessage("user validation service not started. try later.", 10000))
          ()
      }
    case None =>
      h.out(ShutdownMessage("game full. try later.", 10000))
      ()
  }


	// toggle player yank state
	def cmdYank (player : Player, ship: Ship, act : ActorRef) : Boolean = player.shipNameMaybe exists { playerShip =>
    if (ship.unyank) {
      act ! TextMessage("unyanked")
      self ! BroadcastTextMessage(s"$playerShip unyanked.")
      true
    }
    else {
      if (ship.yank) {
        act ! TextMessage("yanked")
        self ! BroadcastTextMessage(s"$playerShip yanked.")
        false
      }
      else {
        act ! TextMessage("already yanked")
        false
      }
    }
  }

	// short command version of sendMsgIfDead
	def sendMsgIfDead(act: ActorRef, _ship: Ship, cmd : Char) : Boolean = sendMsgIfDead(act,_ship,Some(List(cmd)))


	// send the player a message if they try to do a command that is not allowed when dead.
	def sendMsgIfDead(act: ActorRef, ship: Ship, cmd : Option[List[Char]]) : Boolean = {
		if (ship.isDead) {
			cmd foreach {c => msgPlayer(act, s"Command '${c.mkString}' is not operational because you are still dead.") }
		}
		ship.isDead
	}

	def stillDead(act : ActorRef, _ship: Ship): Unit =
		if (_ship.isDead) act ! TextMessage(s"BTW, you are still dead.")

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
						act ! TextMessage(cmdGalacticScan())
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Short Scan
					case 'S' :: rest =>
						msgPlayer(act,cmdShortScan(_player))
						stillDead(act,_ship)
						doCmd (rest)(act,_player,_ship)

					// Fooples display
					case 'F' :: rest =>
						act ! TextMessage(cmdFooples(_player))
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
							val (c, more) = findFirstNotIn(rest, s"$dirs${dirs.toLowerCase}$digits")
							if (!sendMsgIfDead(act,_ship,Some(c))) {
								cmdMove(c)(act, _player, _ship)
							}
							doCmd(more)(act,_player,_ship)
						}
					case 'U' :: rest =>
						val (c, more) = findFirstNotIn(rest, s"$decimals")
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
                    msgPlayer(act, s"Usage: P<ship><fooples>; Cannot phaser Ship '$p'. It is not a valid ship name. Ships are addressed A-Z.")

                  case Some(pp) =>
                    universe.shipByName(pp.toUpper.toString) match {
                      case None =>
                        msgPlayer(act, s"Ship $p is not on.")
                        val (c, more) = findFirstNotIn(pRest, s"$digits")
                        doCmd(more)(act, _player, _ship)

                      case Some(toShip) =>
                        val (c, more) = findFirstNotIn(pRest, s"$digits")
                        if (c.nonEmpty) {
                          // P<player> with some amount
                          if (!sendMsgIfDead(act, _ship, Some(s"P$p${c.mkString}".toList)))
                            cmdPhaser(c.mkString.toInt, toShip)(act, _player, _ship)
                        }
                        else msgPlayer(act,
                          if (_ship.isAlive) s"Usage: P<ship><fooples>; Phasering $toShip requires entering fooples <= ${_ship.fooples}"
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
						val (c, more) = findFirstNotIn(rest, s"$decimals")
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
						msgPlayer(act,s"Unknown Command: $cc")
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
	def receive: PartialFunction[Any, Unit] = {

		// ----
		// A new Player has attempted to join the game
		// Allocate a Ship (if all ships in use. send a Shutdown message and close the socket after 10 secs)
		// Make a new Player actor.
		// Add the Player actor reference to the ship.
		// Start the Player actor
		// Send the Player actor a message with this Game, the handler, and the ship name (A-Z). This last step activates the Player actor.
		//
		case PlayerHandlerRegistrationMessage(handler) =>
			newPlayer(handler)

    // after a player is notified that they have accepted being on they send a message, the game will register their actor
		case PlayerOnMessage(player) =>
			log(s"game registering player ${player.longMessage}")
			playerMap += (sender -> player)

		// ---
		// Handler has indicated that it closed the underlying socket.
		// This means we need to sign the player off.
		//
		case msg: SocketClosedMessage =>
			log(s"socket closed; $msg;  player is being removed")
			getPlayer(sender) foreach { case (act,player) => playerOff(act, player) }

		// ----
		// forward a character from a handler back to a player
		//
		//
		case CharacterFromHandlerMessage(char, handler) =>
			log(s"received a character from handler ${handler.id}")
      getPlayer(handler) foreach { case (act,player) =>
        if (char.toShort == 4) {
          // ctrl-D
          handler.close()
          playerOff(act, player)
        }
        else {
          log(s"sending char $char to player ${player.id}")
          act ! GameCharacterMessage(char)
        }
			}

		// ----
		// Something we can name smacked into the ship associated with player
		//
		case NamedCollisionMessage(body, collidedWith) =>
      getPlayer(collidedWith) foreach {
        case (act,player) =>
          log(player.thisCollidingMessage(body))
          act ! TextMessage(player.collidingMessage(body))
      }

		// ----
		// broadcast message (from a player) to all players except the sending player
		//
		case BroadcastMessage(text) =>
			getPlayer(sender) foreach {
        case (act, player) =>
          log(s"received broadcast: $text from ${player.shipNameMaybe.map {s => s" ship $s"}
            .getOrElse ("unknown ship name")}; id = ${player.id}")

          for {
            (bAct, bPlayer) <- playerMap if bPlayer.id != player.id
          } bAct ! TextMessage(text)
			}

    // ----
    // broadcast message (from the game) to all players
    //
    case BroadcastTextMessage(text) =>
      log(s"received broadcast from Game, broadcast to all players")
      playerMap foreach { case (act, _) =>  act ! TextMessage(text) }

		// ----
		// message from a timer indicating that we should advance the universe
		//
		case _: TimeKeeper =>
			debug("universe advance message from timer")
			val newDate = new Date()
			val interval = newDate.getTime - timeLastAdvance.getTime
			timeLastAdvance = newDate
			this.universe.timeMarchesOn(interval.toDouble)

		// ----
		// message from a timer indicating that we should recharge objects
		//
		case _: RechargeTimer =>
			debug("universe recharge message from timer")
			this.universe.recharge()

		// ----
		// CommandMessage from a player indicating that we should do something with
		// the universe and then send a text message back to the player
		//
		// Anything that messes with the underlying universe needs to message this Game
		// in order for everything to be orderly and synchronized.
		//
		// Players are not allowed to mess with the universe directly.
		//
		case CommandMessage(cmd) =>
      log(s"Game received command: $cmd")
			getPlayer(sender) foreach {
        case (act, player) =>
          log(s"command $cmd from player ${player.id}; ${player.shipNameMaybe.map{ s => s"Ship $s"}.getOrElse("Unknown Ship")}")
          (for {
            shipName <- player.shipNameMaybe
            ship <- universe.shipByName(shipName)
          } yield ship).foreach { ship =>
            doCmd(cmd.trim.toList)(act,player,ship)
          }
	  	}

    case PlayerOffMessage(_, handler: Handler) =>
      getPlayer(handler) foreach { case (act,player) => playerOff (act,player) }

    case CollisionMessage(b) =>
      b match {
        case star: Star =>
          getPlayer(sender) foreach {
            case (act,player) =>
              act ! TextMessage(s"Collision with star of radius ${star.getRadius}.")
              val shipMaybe = for {
                shipName <- player.shipNameMaybe
                ship <- universe.shipByName(shipName)
              } yield ship
              shipMaybe.foreach { ship =>
                ship.collidedWithStar = Option(star)
                cmdMove(Nil)(act,player,ship)
              }
          }
      }

		case GameShutdownMessage(text,time) =>
			playerMap foreach { case (act, _) => act ! GameShutdownMessage(text,time)}

		case CodacasRuntime(g,l) =>
			codacasRuntime = Some(CodacasRuntime(g,l))

		case msg =>
			log(s"unhandled message: $msg")
      log(s"unhandled message class: ${msg.getClass.getName}")
	}
}

object Game {
	def ref : ActorRef = ref(new Game(false))

	def ref(userService: UserValidationService, debug: Boolean): ActorRef =
		actorSystem.actorOf(Props(new Game(debug,Some(userService))))

	def ref(g: Game):ActorRef = actorSystem.actorOf(Props(g))
}
