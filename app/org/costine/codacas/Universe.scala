package org.costine.codacas

import akka.actor.ActorRef

import scala.collection.mutable.ListBuffer
import fi.utu.cs.physics.Point2
import fi.utu.cs.physics.Body
import org.costine.codacas.actors.Player
import scala.language.postfixOps

/**
 * logical Universe
 * 
 * contains a UTU physical universe
 * knows how to safely place ships and other Bodies in the UTU physical universe
 * knows where a "Stargate" to it exists in the "meta-verse"
 * provides a place for ships to exist while being "yanked"
 * contains a star factory for populating stars in the physical universe
 */
class Universe(w:Double, h:Double,
			   _parameters:UniverseParameters,
               _starFactory:StarFactory,
               gateSize:Double,
               metaLoc:Coordinate, 
               metaverse:Universe) extends UniverseObject (gateSize, 0,metaLoc,metaverse) {

  
  private val pUniverse = new CodacasUniverse (w,h)
  
  private val shipPlacer = new ShipPlacer (this)
  private val validShipNames = List (
    "A","B","C","D","E","F","G","H","I","J","K","L","M",
    "N","O","P","Q","R","S","T","U","V","W","X","Y","Z"
  )
  
  
  def width:Double = physicalUniverse.getMaximumCorner.getX
  def height:Double = physicalUniverse.getMaximumCorner.getY
  
  def universearea = {width * height}
  
  def physicalUniverse = { 
    val _u = pUniverse
    _u.setAccelerationOfGravity(0.0)
    _u
  }
  
  def parameters = _parameters
  
  // ship will have this quantity of fooples to start
  def initialFooples = parameters.initialFooples
  
  // ship will have this amount of WQL to start
  def initialWql = parameters.initialWql
  
  // ship's fooples will recharge at these many per sec
  def foopleRechargeRate = parameters.foopleRechargeRate
  
  // ship's wql will recharge at these many per sec
  def initialWqlRR = parameters.initialWqlRR
  
  // ship's max fooples will be this number
  def initialMaxFooples = parameters.initialMaxFooples
  
  // phaser fooples times this value will be the amount of fooples that
  // the ship will use to fire the phasers. eg. with it set to .5, a PA100
  // will subtract 50 fooples from the ship. It's the cost of firing them.
  def phaserFoopleFactor = parameters.phaserFoopleFactor
  
  // phasers will dropoff at this rate * the distance.
  def phaserDropoffFactor = parameters.phaserDropoffFactor
  
  // ship will have these number of maximum torps
  def initMaxShipTorps = parameters.initMaxShipTorps
  
  // torps will be replenished at these number per second
  def initTorpReplenishRate = parameters.initTorpReplenishRate
  
  // initial torp mass
  def initTorpMass = parameters.torpMass
  
  // initial torp size
  def initTorpSize = parameters.torpSize
  
  // torps won't be replaced until the ships fooples go above this
  def torpFirePct = parameters.torpFirePct
  
  // ship will have these number of maximum mines
  def initMaxShipMines = parameters.initMaxShipMines
  
  // ship available mines will be replaced at these number per second 
  // up to the max.
  def initMineReplenishRate = parameters.initMineReplenishRate
  
  // ship available mines won't start to get replaced until the ships
  // fooples reach this number.
  def minMineReplenishFpl = parameters.minMineReplenishFpl
  
  // list of yanked ships
  var yanked: ListBuffer[Ship] = new ListBuffer[Ship]
  
  def validShipName(_name:String) = {
    validShipNames.contains(_name)
  }
  
  def add(o:UniverseObject):Boolean = {
    val loc = o.location
    if (isPlaceable(o)) {
      if (o.isInstanceOf[Ship]) {
    	 val _s = o.asInstanceOf[Ship]
    	 if (shipByName(_s.name) == null) {
    	   physicalUniverse.addBody(o)
    	 }
      }
      else {
         physicalUniverse.addBody(o)
      }
      true
    }
    false
  }
  
  def newShip () = {
    var l: ListBuffer[Ship] = new ListBuffer[Ship]
    for(sn <- validShipNames) {
      if (l.size == 0 && (shipByName(sn) == null)) {
        l+=new Ship(sn, .5, .5,new Coordinate(0,0), this)
      }
    }
    if (l.size > 0) {
    	val s = l.apply(0)
    	placeShip(s)
    	s
    }
    else {
      null
    }
    
  }
  
  def newShip (_p:ActorRef): Ship = {
    val _s = this.newShip
    _s.player(_p)
    _s
  }
  
  def remove (o:UniverseObject) = {
    if (list.contains(o)) {
      physicalUniverse.removeBody(o)
      true
      }
    else {
      false
    }
  }
  
  def starFactory = _starFactory 
  
  def list ():List[UniverseObject] = {
    var l: ListBuffer[UniverseObject] = new ListBuffer[UniverseObject]
    val it:java.util.Iterator[_] = physicalUniverse.bodies.iterator
    while (it.hasNext) {
      val o = it.next
      if (o.isInstanceOf [UniverseObject]) {
        l += o.asInstanceOf[UniverseObject]
      }
    }

    l.toList
  }
  
  def isPlaceable(o:UniverseObject):Boolean = {
    val loc = o.location
    if (! ((loc.x >= 0 && loc.x <= width) && 
          (loc.y >= 0 && loc.y <= height)))
        return false
    
    for (uo <- list) {
    	if (o != uo) {
	    	if (o.isOverlapping(uo)) {
	    	  return false
	    	}
    	}
    }
    return true

  }
  
  def place (o:UniverseObject) = {
    while (!isPlaceable(o)) {

      var ox = o.location.x
      if (ox > width || ox < 0) {
        ox = o.size
      }
      else {
        ox += o.size
      }
      
      var oy = o.location.y
      if (oy > height || oy < 0) {
        oy = o.size
      }
      else {
        oy += o.size
      }
      
      o.location(new Coordinate (ox,oy))

    }
  }
  
  def accept (uo:UniverseObject) = {
   	  place(uo)
 	  add(uo)
  }
  
  def repopulateStars () = {
    clearStars
    starFactory.universe(this)
    starFactory.makeStars
    starFactory.get
  }
  
  def clearStars () = {
    for (body <- list) {
      if (body.isInstanceOf[Star]) {
    	physicalUniverse.removeBody(body)
      }
    }
  }
  
  def stars () = {
    var l:ListBuffer[Star] = new ListBuffer[Star]
    for (body <- list) {
      if (body.isInstanceOf[Star]) {
    	l += body.asInstanceOf[Star]
      }
    }
    l
  }
  
  def torps = {
    var l:ListBuffer[Torp] = new ListBuffer[Torp]
    for (body <- list) {
      if (body.isInstanceOf[Torp]) {
    	l += body.asInstanceOf[Torp]
      }
    }
    l
  }
  
  def torps (_ship:Ship):ListBuffer[Torp] = {
    var l:ListBuffer[Torp] = new ListBuffer[Torp]
    for (body <- torps) {
      if (body.ship == _ship) {
    	l += body
      }
    }
    l
  }
  
  // return complete list of yanked and unyanked ships
  def ships () = {
    var l:ListBuffer[Ship] = new ListBuffer[Ship]
    for (body <- list) {
      if (body.isInstanceOf[Ship]) {
    	l += body.asInstanceOf[Ship]
      }
    }
    for (body <- yanked) {
      l+=body
    }
    l
  }
  
  def yank(_ship:Ship):Boolean = {
	  if (yanked.contains(_ship)) {
	    false
	  }
	  else {
		 removeShip(_ship.name)
		 _ship.yanked = true
		 yanked+=_ship
		 true
	  }
  }
  
  def unyank(_ship:Ship):Boolean = {
    if (yanked.contains(_ship)) {
      yanked-=_ship
      placeShip(_ship)
      _ship.yanked = false
      true
    }
    else {
      false
    }
  }
  
  def recharge() = {
    list.foreach( obj => obj.recharge())
  }
  
  
  
  // find all of the objects in the rectangular section of the universe bounded
  // by upper left corner (_x and _y), and lower right corner (_xe and _ye)
  def locateObjectsInGrid(_x:Double, _xe:Double, _y:Double, _ye:Double):List[UniverseObject] = {
    var l: ListBuffer[UniverseObject] = new ListBuffer[UniverseObject] 
    for (uo <- list) {
    	if (inGrid (uo,_x,_xe,_y, _ye))
    		l+=uo     
    }
    l toList
  }
  
  def inGrid (_uo:UniverseObject, _x:Double, _xe:Double, _y:Double, _ye:Double):Boolean = {
    	if (_x > _xe) {
    	  	
    	  if (_y > _ye) {
    	    
    	    // in the corners
    	    inGrid(_uo,0,_xe,0,_ye) || 			// top left
    	    inGrid(_uo,_x,width,0,_ye) || 		// top right
    	    inGrid(_uo,0,_xe,_y,height) ||		// bottom left
    	    inGrid(_uo,_x,width,_y,height)		// bottom right
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
    	    inGrid (_uo,_x,_xe,_y,height) ||
    		inGrid (_uo,_x,_xe,0,_ye)
    	  }
    	  else {
    		 // somewhere in the middle
    		 val uox = _uo.location.x
    		 val uoy = _uo.location.y
    		 (uox >= _x) && (uox < _xe) && (uoy >= _y) && (uoy < _ye)
    	  }
    	}
  }
  
  def placeShip (_s:Ship) = {
    shipPlacer.place(_s)
  }
  
  def shipByName (_s:String):Ship = {
	  val l = (for {
		  ss <- ships // a generator
		  n = ss 	 // a definition
		  if (n.name == _s) // a filter
	  } yield n)
	  if (l.size > 0 ) {
	    l.apply(0)
	  }
	  else {
		  null
	  }
  }
  
  def shipOff (_ship:String) = {
  		val _s = shipByName(_ship)
		if (yanked.contains(_s)) {
			yanked-=_s
		}
		else {
			removeShip(_ship)
		}
  }
  
  def removeShip (_s:String) = {
    physicalUniverse.removeBody(shipByName(_s))
  }
  
  def wrapWidth (_x: Double) = {
    wrapCoord(_x,width)
  }
  
  def wrapHeight (_y: Double) = {
	 wrapCoord(_y,height)
  }
  
  def wrapCoord(_c:Double, _s:Double) = {
     if (_c > _s) {
	    _c - _s
	 }
	 else if (_c < 0){
	    _s + _c
	 }
	 else {
		_c
	 }
  }

  def timeMarchesOn (dt:Double) = {
    physicalUniverse.advance(dt)
  }
}
