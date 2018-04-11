package org.costine.codacas

import org.costine.codacas.renderers.CharacterArrayRenderer
import org.costine.codacas.renderers.ShortRangeCharacterArrayRenderer

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

object UniverseDump  {

	private var currentShip: Option[Ship] = None
  private var uni: Universe = _

	def usingShip (ship: Ship): Unit = { currentShip = Some(ship) }

  def stopUsingShip () : Unit = {currentShip = None}

	def usingShip: Option[Ship] = currentShip

	def universe (_u:Universe): Unit = {uni = _u}

	def universe: Universe = uni

	def newUniverse: Universe = {

		universe(
      new Universe(
        200, 	// width
        200, 	// height
        new UniverseParameters (),
        new StarFactory(.02, 1, 3, 1000, 2000, 50, 4000), // how the universe populates stars
        5,						// stargate size (the size of this universe in the parent)
        new Coordinate (0,0), 	// stargate location (loc of universe in parent)
        null						// parent universe
		  )
    )

		// (re)-populate all of the stars
		universe.repopulateStars()
	}

	def cmdUse (s:String): String = universe.shipByName(s).map { ship =>
    usingShip (ship)
    "Using ship " + ship.name
  } getOrElse showShipsOrLogonMsg

	def cmdGalacticScan(): String = new CharacterArrayRenderer (this.universe, 20, " ", "\n").toString


	def cmdShortScan(): String =
    usingShip.map { ship =>
      new ShortRangeCharacterArrayRenderer (ship, 40, 11, " ", "\n").toString + "\n" + ship
    } getOrElse showShipsOrLogonMsg

	def showShipIds: String = universe.ships().map { _.name } mkString ","

	def showShipsOrLogonMsg: String =
		if (showShipIds.nonEmpty) "U[Ship], where Ship is: " + showShipIds else "\"logon\" to use a ship"

	def response(cmd:String): String = {
      cmd match {
        case "LOGOFF" =>
          usingShip.map { ship =>
            if (this.universe.remove(ship)) this.universe.ships() match {
              case Nil =>
                stopUsingShip()
                showShipsOrLogonMsg
              case first :: _ => ship.name + " is off.\n" + cmdUse(first.name)
            } else showShipsOrLogonMsg
          } getOrElse showShipsOrLogonMsg

        case "LOGON" =>
          this.universe.newShip().map { ship => cmdUse(ship.name)} getOrElse "Game full. Try again later."

        case other =>
          cmd.toCharArray.toList match {
            case Nil => cmdShortScan()
            case action :: rest =>
              action match {
                case 'U' =>
                  rest match {
                    case Nil =>
                      usingShip.map { ship =>
                        s" ${ ship.name } is on. Ships: " + showShipIds
                      } getOrElse showShipsOrLogonMsg
                    case shipName :: restUse =>
                      cmdUse(shipName.toString)
                  }
                case 'S' => cmdShortScan()
                case 'V' => usingShip.map { ship => s"${ship.name}: $ship"} getOrElse showShipsOrLogonMsg
                case 'G' => cmdGalacticScan()
                case 'H' =>
                  usingShip.map { ship =>
                    new HyperSpacer (this.universe).place(ship)
                    cmdShortScan()
                  } getOrElse showShipsOrLogonMsg
                case 'W' =>
                  usingShip.map { ship =>
                    rest match {
                      case Nil =>
                        ship.setVelocity(0, 0)
                      case dir :: amtRest =>
                        val amt = (Try { Integer.parseInt(amtRest.mkString) } getOrElse 0)/1000.0
                        val vel = ship.getVelocity
                        dir.toUpper match {
                          case 'U' => vel.setDeltaX(-amt)
                          case 'D' => vel.setDeltaX(amt)
                          case 'L' => vel.setDeltaY(-amt)
                          case 'R' => vel.setDeltaY(amt)
                          case _ => ()
                        }
                        ship.setVelocity( vel)
                        ship.setAcceleration(0,0)
                    }
                    ""
                  } getOrElse showShipsOrLogonMsg
                case _ => cmdShortScan()
              }

          }

      }
    }

  @tailrec def run(cmd: () => String, date: java.util.Date) : Unit = cmd() match {
    case "Q" => ()
    case other =>
      println("-------------------------------------------------------------")
      println(response(other))
      this.universe.timeMarchesOn((new java.util.Date).getTime - date.getTime)
      run(cmd,new java.util.Date())
  }

	newUniverse
	run({ () => StdIn.readLine.trim.toUpperCase },new java.util.Date)

}
