package org.costine.codacas

import akka.actor.ActorRef

import scala.collection.mutable.ListBuffer
import fi.utu.cs.physics.Point2
import fi.utu.cs.physics.Body
import org.costine.codacas.actors.Player

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Try

/**
 * logical Universe
 * 
 * contains a UTU physical universe
 * knows how to safely place ships and other Bodies in the UTU physical universe
 * knows where a "Stargate" to it exists in the "meta-verse"
 * provides a place for ships to exist while being "yanked"
 * contains a star factory for populating stars in the physical universe
 */
class Universe(w:Double, h:Double, _parameters:UniverseParameters, _starFactory:StarFactory,
         gateSize:Double, metaLoc:Coordinate, metaverse: Universe)
  extends UniverseObject (gateSize, 0,metaLoc, metaverse) {

  private val pUniverse = new CodacasUniverse (w,h)

  private val shipPlacer = new ShipPlacer (this)
  val validShipNames = List (
    "A","B","C","D","E","F","G","H","I","J","K","L","M",
    "N","O","P","Q","R","S","T","U","V","W","X","Y","Z"
  )

  def width:Double = physicalUniverse.getMaximumCorner.getX
  def height:Double = physicalUniverse.getMaximumCorner.getY

  def universearea: Double = width * height

  def physicalUniverse: CodacasUniverse = {
    val _u = pUniverse
    _u.setAccelerationOfGravity(0.0)
    _u
  }

  def parameters: UniverseParameters = _parameters

  // ship will have this quantity of fooples to start
  def initialFooples: Int = parameters.initialFooples

  // ship will have this amount of WQL to start
  def initialWql: Double = parameters.initialWql

  // wqls dropoff at this rate when ship is moving
  def wqlDropoffFactor: Double = parameters.wqlDropoffFactor

  // ship's fooples will recharge at these many per sec
  def foopleRechargeRate: Double = parameters.foopleRechargeRate

  // ship's wql will recharge at these many per sec
  def initialWqlRR : Double = parameters.initialWqlRR

  def wqlReqForMove : Double = parameters.wqlReqForMove

  // ship's max fooples will be this number
  def initialMaxFooples: Int = parameters.initialMaxFooples

  // phaser fooples times this value will be the amount of fooples that
  // the ship will use to fire the phasers. eg. with it set to .5, a PA100
  // will subtract 50 fooples from the ship. It's the cost of firing them.
  def phaserFoopleFactor: Double = parameters.phaserFoopleFactor

  // phasers will dropoff at this rate * the distance.
  def phaserDropoffFactor: Double = parameters.phaserDropoffFactor

  // ship will have these number of maximum torps
  def initMaxShipTorps: Double = parameters.initMaxShipTorps

  // torps will be replenished at these number per second
  def initTorpReplenishRate: Double = parameters.initTorpReplenishRate

  // initial torp mass
  def initTorpMass: Double = parameters.torpMass

  // initial torp size
  def initTorpSize: Double = parameters.torpSize

  // torps won't be replaced until the ships fooples go above this
  def torpFirePct: Double = parameters.torpFirePct

  // ship will have these number of maximum mines
  def initMaxShipMines: Int = parameters.initMaxShipMines

  // ship available mines will be replaced at these number per second 
  // up to the max.
  def initMineReplenishRate: Double = parameters.initMineReplenishRate

  // ship available mines won't start to get replaced until the ships
  // fooples reach this number.
  def minMineReplenishFpl: Int = parameters.minMineReplenishFpl

  // W command multiplier
  // the higher this number the faster your WQLs will run out
  // default is 0.001, but override by player with special command ":warp magnitude <value>"
  def wCommandMagnitudeFactor: Double = parameters.wCommandMagnitudeFactor

  // list of yanked ships
  var yanked: ListBuffer[Ship] = new ListBuffer[Ship]

  // return if it is a valid ship name
  def validShipName(_name:String): Boolean = validShipNames.contains(_name)

  def existsPhysically(universeObject: UniverseObject): Boolean =
    physicalUniverse.bodies().contains {universeObject}

  def add(o:UniverseObject): Boolean = {
    if (isPlaceable(o)) {
      physicalUniverse.addBody(o)
      existsPhysically(o)
    }
    else
      false
  }


  // get first unused ship name
  def newShip (): Option[Ship] = {
    val newShipMaybe = validShipNames.find { sn => shipByName(sn).isEmpty}
      .map { emptyShipName => new Ship(emptyShipName, .5, .5,new Coordinate(0,0), this) }

    newShipMaybe.map { s => placeShip(s)}.flatMap {
      case true => newShipMaybe
      case false => None
    }
  }

  def newShip (_p:ActorRef): Option[Ship] =
    newShip().map {_s => _s.player(_p); _s}

  def remove (o:UniverseObject): Boolean =
    list().find { _.equals(o)}.exists { oo =>
      physicalUniverse.removeBody(o)
      true
    }

  def starFactory: StarFactory = _starFactory

  def list ():List[UniverseObject] = {
    import scala.collection.JavaConverters._
    physicalUniverse.bodies.iterator.asScala.toList.collect {
      case o: UniverseObject => o
    }
  }

  def between(loc: Double, low: Double, high: Double): Boolean = loc >= low && loc <= high
  def between(loc: Coordinate): Boolean = between(loc.x,0,width) && between(loc.y,0,height)


  // must be a value in the Universe size and not overlapping with anything else
  def isPlaceable(o:UniverseObject):Boolean =
    if (! between(o.location)) false
    else list().find { uo => (uo != o) && o.isOverlapping(uo)}.forall{ _ => false }

  def place(o:UniverseObject): UniverseObject = {

    @tailrec def findPlace(o: UniverseObject): UniverseObject = {
      if (isPlaceable(o)) o
      else {
        val _ox = o.location.x
        val _oy = o.location.y
        o.location(
          new Coordinate(
            if (_ox > width || _ox < 0) o.size else _ox + o.size,
            if (_oy > height || _oy < 0) o.size else _oy + o.size
          )
        )
        findPlace(o)
      }
    }

    findPlace(o)
  }

  def accept (uo:UniverseObject): Boolean = add(place(uo))

  // repopulate all of the stars in the universe
  def repopulateStars (): Universe = {
    clearStars()
    starFactory.universe(this)
    starFactory.makeStars()
    starFactory.get()
    this
  }

  // remove all the stars
  def clearStars (): Unit = stars().foreach {s => physicalUniverse.removeBody(s)}

  // return all stars
  def stars (): List[Star] = list().collect { case s: Star => s }

  // return all torps
  def torps: List[Torp] = list().collect { case t: Torp => t}

  // return all torps for a particular ship
  def torps (_ship:Ship):List[Torp] = torps.filter { _.ship == _ship}
  
  // return complete list of yanked and unyanked ships
  def ships(): List[Ship] =
    list().collect {
      case ship: Ship => ship
    } ++ yanked.toList

  def yank(_ship:Ship):Boolean = yanked.find { _.equals(_ship)} match {
    case Some(_) => false
    case None =>
      removeShip(_ship.name)
      _ship.yanked = true
      yanked+=_ship
      true
  }


  // put the yanked ship back into the game
  def unyank(_ship:Ship):Boolean = yanked.find { _.equals(_ship)} match {
      case Some(s) =>
        yanked-=s
        placeShip(s)
        s.yanked = false
        true
      case None => false
    }

  // do a "recharge" on everything
  def recharge(): Unit = list().foreach( obj => obj.recharge())

  // find all of the objects in the rectangular section of the universe bounded
  // by upper left corner (_x and _y), and lower right corner (_xe and _ye)
  def locateObjectsInGrid(_x:Double, _xe:Double, _y:Double, _ye:Double):List[UniverseObject] =
    list().filter { uo => inGrid (uo,_x,_xe,_y, _ye)}
  
  def inGrid (_uo:UniverseObject, _x:Double, _xe:Double, _y:Double, _ye:Double):Boolean = {
    if (_x > _xe) {
      if (_y > _ye) {
        // in the corners
        inGrid(_uo,0,_xe,0,_ye) || 			// top left
    	  inGrid(_uo,_x,width,0,_ye) || 	// top right
    	  inGrid(_uo,0,_xe,_y,height) ||	// bottom left
    	  inGrid(_uo,_x,width,_y,height)	// bottom right
      }
    	else {
        // along the sides
    		inGrid(_uo,_x,width,_y,_ye) ||
    		inGrid(_uo,0,_xe,_y,_ye)
    	}
    }
    else {
      // along top and bottom
      if (_y > _ye) {
        inGrid (_uo,_x,_xe,_y,height) || inGrid (_uo,_x,_xe,0,_ye)
      }
      else {
        // somewhere in the middle
        val uox = _uo.location.x
        val uoy = _uo.location.y
        (uox >= _x) && (uox < _xe) && (uoy >= _y) && (uoy < _ye)
      }
    }
  }
  
  def placeShip (_s:Ship): Boolean = shipPlacer.place(_s)

  def shipByName (_s:String):Option[Ship] = ships().find { _.name == _s }

  def shipOff (_ship:String): Unit = shipByName(_ship).foreach { _s =>
    yanked.find { _.equals(_s)}.map { _ss => yanked -= _ss}
    removeShip(_ship)
  }
  
  def removeShip (_s:String): Unit = shipByName(_s).foreach { physicalUniverse.removeBody(_)}
  
  def wrapWidth (_x: Double): Double = wrapCoord(_x,width)

  def wrapHeight (_y: Double): Double = wrapCoord(_y,height)
  
  def wrapCoord(_c:Double, _s:Double): Double =
    if (_c > _s) _c - _s
	  else if (_c < 0) _s + _c
	  else _c

  def timeMarchesOn (dt:Double): Unit = physicalUniverse.advance(dt)

}
