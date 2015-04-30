package org.costine.codacas

import akka.actor.ActorRef
import org.costine.codacas.interactions.{Phaser,PhaserResult}
import org.costine.codacas.actors.Player
import fi.utu.cs.physics.{Vector2, Body, Point2}
import java.util.Date
import scala.collection.mutable.ListBuffer

/**
 * Ship
 */
class Ship(id:String, m:Double, s:Double, loc:Coordinate, u:Universe)
  extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe)
 with Naming {
  
  var lastChargeTime = new Date ().getTime
  
  // count of torps that are ready to fire
  var torps = u.parameters.initShipTorps
  
  var defaultTorpRange = 100.0
  var defaultTorpSpeed = 0.01
  var defaultTorpFuel:Long = 5000 // 5 secs of fuel
  var defaultTorpPayload = 200 
  var defaultTorpAngle = 0.0 
  
  var defaultTorpMass = u.initTorpMass
  var defaultTorpSize = u.initTorpSize
  
  var fooples = u.initialFooples
  var wql : Double = u.initialWql
  def foopleRechargeRate = u.foopleRechargeRate
  def torpReplenishRate = u.initTorpReplenishRate
  var wqlRechargeRate = u.initialWqlRR
  
  var maxFooples = u.initialMaxFooples
  var maxTorps = u.initMaxShipTorps
  var maxMines = u.initMaxShipMines
  
  var yanked = false
  
  var owner:String = null
  
  private var _player:ActorRef = null

  var collidedWithStar : Option[Star] = None

  def log (_x:String) = {
    println ("Ship " + id + ": " + _x)
  }

  // pushing up against a star requires us to keep the
  // last star around, and get the distance from it.
  def distanceToCollidedStar : Option[Double] = {
    collidedWithStar map {  cws =>
        distance(cws)
    }
  }
  
  /**
   * restart this ship
   */
  def restart = {
	  defaultTorpRange = 100.0
	  defaultTorpSpeed = 0.01
	  defaultTorpFuel = 5000 // 5 secs of fuel
	  defaultTorpPayload = 200 
	  defaultTorpAngle = 0.0
	  fooples = u.initialFooples
	  wql = u.initialWql
	  wqlRechargeRate = u.initialWql
    collidedWithStar = None
  }
  
  def player = _player
  
  def player (_p:ActorRef) = {
    _player = _p
  }
  
  /**
   * A ship is dead when the fooples are <= 0
   */
  def isDead () = {
    fooples <= 0
  }
  
  /**
   * A ship is alive when it isn't dead
   */
  def isAlive () = {
    !isDead
  }
  
  /**
   * phaser factor:
   * 
   * divide by this number to figure out many real fooples to subtract from the
   * firing ship. This will be the cost of firing the phasers to the ship doing the
   * shooting.
   * 
   * It is the universe-wide value.
   */
  def phaserFoopleFactor = u.phaserFoopleFactor
  
  /**
   * phaser dropoff factor:
   * 
   * distance between ships multiplied by this.
   * if supplied fooples = 100 and this is .1 then effective phaser hit will be
   * 100 / distance * .1
   * 
   * It is the universe-wide value.
   */
  def phaserDropoffFactor = u.phaserDropoffFactor
  
  /**
   * The ship name is the id
   */
  def name:String = id
  
  /**
   * charge fooples, torps
   */
  def recharge () = {
    val now = new Date ().getTime
    val interval = now - lastChargeTime
    val intervalSecs = interval / 1000
    lastChargeTime = now
    
    // ----
    // check if foople recharging required
    //
    if ((fooples < maxFooples) && isAlive()) {
      val newfooples = (fooples + (foopleRechargeRate * intervalSecs)).toInt
      if (newfooples >= maxFooples) {
        fooples = maxFooples
        // inform player
        player ! ("msg", "fpl recharge complete")
      }
      else {
        fooples = newfooples
      }
    }
    
    // ----
    // check if torps need recharging
    //
    if (torps < 0) {
      torps = 0
    }
    
    // only recharge if we are alive.
    if ((torps < maxTorps) && isAlive()) {
      val newtorps = torps + (torpReplenishRate * intervalSecs)
      if (newtorps >= maxTorps) {
        torps = maxTorps
        // inform player
        player ! ("msg", "torps at maximum again")
      }
      else {
        torps = newtorps
      }
    }

  }
  
  /**
   * incur a phaser hit from another ship s.
   * Amount of hit is based on the phaserDropoffFactor * distance
   */
  def phaserHit(s:Ship,f:Int):Int = {
    val _dist = s.distance(this)
    var _dof = phaserDropoffFactor
    if (_dof == 0) {
      _dof = 1
    }
    
    var _n = (_dist * _dof ).intValue
    if (_n == 0) {
      _n = 1
    }
    
    var hitWith = f / _n
    if (hitWith <= 0) {
      hitWith = 0
    }
    else {
      fooples = fooples - hitWith
    }
    s.fooples = s.fooples - f
    hitWith
  }
  
  
  /**
   * fire phasers onto ship s
   */
  def phaser(s:Ship, f:Int):Result = {
    new Phaser (this,s,f).interact
  }

  /**
   * place the torp relative to the ship depending on the angle.
   *
   *			 90
   *
   *		180	  .   0
   *
   *			270
   *
   * 
   */
  def placeTorp ( _velocity:Velocity, _size:Double ):Coordinate = {
    val _sx = location.x
    val _sy = location.y
    val _ssize = size
    
	val _vx = Math.sin(Math.toRadians(_velocity.angle)) * ((_ssize) + (_size*2))
	val _vy = Math.cos(Math.toRadians(_velocity.angle)) * ((_ssize) + (_size*2))
 
	val _nx = _sx + _vx
	val _ny = _sy + _vy
 
	val _np = universe.physicalUniverse.transpose (new Point2 (_nx,_ny))
 
    new Coordinate(_np.getX,_np.getY)
  }

  /**
   * Fire the torp, it has a foople payload of _fooples, and an explosive
   * range of _range. It will deliver it's full payload of _fooples if it
   * hits another ship. If the fuel reaches 0, it will also detonate and 
   * affect any ships in the explosive range. The amount a ship is affected is
   * determined by its distance from the explosion using phaser rules. The
   * closer the ship is to the explosion, the more fooples will be lost.
   *
   * A torp will misfire if it cannot be placed because something else is in
   * the way. A misfired torp is a lost torp.
   */
  def fireTorp ( _velocity:Velocity, _fooples:Int, _range:Double) = {
    if (_fooples <= (fooples * universe.torpFirePct)) {
    	// gotta have at least a whole torp to fire
	    if (torps >= 1.0) {
	    	// payload adds to the mass of the torp
	    	val torpMass = defaultTorpMass * _fooples
	    	// size determines how easy a hit will be
	    	val torpSize = defaultTorpSize
		    val torp = new Torp (_fooples,_velocity.timeUnits,_range,this,torpMass,torpSize,placeTorp(_velocity,torpSize),universe)
		    torps = torps - 1.0
		    fooples -= _fooples
		    torp.setVelocity(_velocity.velocity)
		    if (universe.isPlaceable(torp)) {
		      universe.add(torp)
		      new Result(0,"torp fired at angle " + _velocity.angle)
		    }
		    else {
		      new Result(1,"torp mis-fired")
		    }
	    }
	    else {
	      new Result (2,"out of torps")
	    }
    }
    else {
      new Result (3,"cannot deploy torp; current fpl = " + fooples + 
                    "; max torp payload = " + (fooples * universe.torpFirePct) +
      				"; current torp payload setting = " + defaultTorpPayload )
    }
  }

  /**
   * Fire torp using default parameters and last angle
   */
  def fireTorp:Result = {
	fireTorp (defaultTorpAngle)
  }
  
  /**
   *  Fire torp using default parameters
   */
  def fireTorp(_angle:Double):Result = {
    defaultTorpAngle = _angle
	val _velocity = new Velocity (defaultTorpSpeed,defaultTorpFuel,_angle)
    fireTorp ( _velocity, defaultTorpPayload,defaultTorpRange)    
  }
  
  /**
   * Direct hit by torp - result is the number of fooples the Ship
   * is left with
   */
  def torpHit (_torp:Torp) = {
	val _fpls = _torp.payload
	fooples -= _fpls
  }
  
  /**
   * Torp didn't hit anything but exploded after running out of fuel.
   */
  def torpExploded(_torp:Torp) = {
    val _dist = this.distance(_torp)
    if (_dist < _torp.range) {
	    var _dof = phaserDropoffFactor
	    if (_dof == 0) {
	      _dof = 1
	    }
	    
	    var _n = _dist * _dof 
	    if (_n == 0) {
	      _n = 1
	    }
	    
	    var hitWith = (_torp.payload / _n).intValue
	    if (hitWith <= 0) {
	      hitWith = 0
	    }
	    else {
	      fooples = fooples - hitWith
	    }
	    fooples -= hitWith 
	    hitWith
    }
    else {
      0
    }
  }

  // create a snapshot of a ship to compare with before the universe changed
  def snap : Ship = {
     val x = new Ship(id, m, s, loc.snap, u)
      x.setPosition(getPosition)
      x.setVelocity(getVelocity)
      x
  }


  // handle how the ship will move through the universe and perform wql recharging
  // based on how much the ship is attempting to move
  override def advance (dt: Double) : Unit = {

    //log (s"Time passed for ship: ${dt} msec")
    val starting = snap

    // these many are required for movement
    val req = u.wqlReqForMove  * starting.getVelocity.length

    // if wql = 0 freeze the ship while the universe inverts
    if (wql <= req ) {
      setVelocity(0.0, 0.0)
      log(s"ship is frozen ${wql} <= ${req}")
    }

    // advance the ship in the universe
    super.advance(dt)

    // find the distance covered
    val covered = distance(starting)
    if (covered > 0.0) {
      log(s"Covered ${covered} units")


      // reduce the WQLs by the amount of distance covered but not negative
      wql = math.max(0.0, math.min(u.initialWql, wql - (covered * u.wqlDropoffFactor) ))

      log(s"wql after travelling: ${wql}")
    }

    // put old velocity back if wqls less than wqls required for moving
    if (wql <= req ) setVelocity(starting.getVelocity)

    // get distance to "collided with" star
    val ds = distanceToCollidedStar filter(d => d> 0.0)

    // Wql refresh rate depends on the density of the star
    val cccsr = collidedWithStar filter {ss => ss.getRadius > 0}
    val ccsr = cccsr map { star => (star.density.getOrElse(500.0)) / wqlRechargeRate }
    val csr = ccsr.getOrElse(1.0)

    // you get "these many more" based on wqlRechargeRate, the time passed, and the density of the pushed against star
    val moreWql = ((dt / 1000.0) * wqlRechargeRate) * ( csr * (1.0 / (ds getOrElse(1.0))) )
    //log(s"distance to collided with star = ${ds}, getting ${moreWql} more WQLs")

    wql = math.min(u.initialWql,wql + moreWql)
  }

  /**
   * let the ship's player know that a collision has occurred with the Body.
   */
  def informPlayerCollision (_body:Body) {

	  // is the other body a ship? if so let that ship's player know
	  if (_body.isInstanceOf[Ship]) {
		  _body.asInstanceOf[Ship].player ! ("collide",this)
	  }
	  else if (_body.isInstanceOf[Torp]){
	    val _torp = _body.asInstanceOf[Torp]
	    _torp.collidesWith(this)
	  }
  }
  
  /**
   * Yank the ship from the universe, no interaction is allowed
   */
  def yank = {
    universe.yank(this)
  }
  
  /**
   * put the ship back into the game, interactions will be allowed again
   */
  def unyank = {
    universe.unyank(this)
  }
  
  /**
   * send a message to this Ship's Player when this ship has an inelasticCollision
   */
  override def inelasticCollision (_body:Body) = {
	  informPlayerCollision (_body)
    super.inelasticCollision(_body)
  }
  
  /**
   * send a message to this Ship's Player when this ship has an elasticCollision
   */
  override def elasticCollision (_body:Body) = {
    informPlayerCollision (_body)
    super.elasticCollision(_body)
  }

  def longMessage = s"ship ${name}"

  def shortMessage = s"${name}"

  // ship torp status
  def torpInfoMessage(p: String, s: String) : String = {
    List(
      s"${p}N=${torps.intValue}",
      s"Speed: ${defaultTorpSpeed}",
      s"Range: ${defaultTorpRange}",
      s"Payload: ${defaultTorpPayload}",
      s"Fuel: ${defaultTorpFuel}"
    ).mkString(s)
  }

  override def toString : String = {

    val d = distanceToCollidedStar.map {dc => s"; Distance: ${dc}"}.getOrElse("")

    List(
      s"Fpls: ${fooples}",
      s"WQL: ${wql}",
      s"Torps: ${torps.toInt}",
      s"Pos(x,y): ${getPosition}",
      s"Vel(x,y): ${getVelocity}"
    ).mkString("; ") +
      collidedWithStar.map {star => s"Star: [${star}${d}}]"}.getOrElse("")
  }
}
