package org.costine.codacas

import org.costine.codacas.actors.Player
import fi.utu.cs.physics.Body
import java.util.Date
class Torp(_payload:Int, _fuel:Long, _range:Double, _ship:Ship, m:Double, s:Double, loc:Coordinate, u:Universe)
	extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe){

  // null payload means that torp has not been launched.
  def payload = _payload
  
  // fuel - when fuel reaches 0, torp explodes
  var fuel = _fuel
  
  // effective explosive distance, ships within this distance will be affected
  // when the torp explodes. payload fooples will be delivered.
  def range = _range
  
  var ship = _ship
  
  // when the torp hits a ship, set this so that the explosion doesn't affect the
  // hit ship - it already received damage from the payload
  var hitShip:Ship = null
  
  var lastRecharge = new Date().getTime
  
  // updates the torp after it is launched - in this case a "recharge" means to
  // remove fuel. fuel is the number of ms that the torp will last.
  // when it gets to 0 ships players within range will be notified 
  def recharge() = {
	  val _d = new Date().getTime - lastRecharge
	  if (_d > 0) {
		  fuel -= _d
	  }
   
	  // ran out of fuel - time to detonate, and then remove ourself from the
	  // universe.
	  if (fuel <= 0) {
	    exploded     
	  }
	  else {
		  lastRecharge = new Date().getTime
	  }
	  
  }
  
  def exploded = {

	  // go through all the ships and let them know there might
	  // be damage
        var _totalHit = 0
        var _damage = 0
  	    for (body <- universe.ships) {
  	    	// the "hit" ship doesn't incur damage from the torp that hit it
  	    	if (!(body.yanked) && body.isAlive) {
	  	    	if (hitShip != body) {
							val _hit = body.torpExploded(this)
							if (_hit > 0) {
								if (body != this.ship) {
									_totalHit += _hit
								}
								else {
									_damage = _hit
								}
								body.player ! ("exploded", this, _hit)
			    		}
		    		}
					}
	    }
	    this.setVelocity(0.0,0.0)
	    universe.remove(this)
	    val _t = _totalHit+_damage
	    if ( (_t <= 0) && (hitShip == null)) {
	      this.ship.player ! ("msg", "Torp Missed")
	    }
	    else {
	    	if (_t > 0) {
	    		this.ship.player ! ("msg", "Torp exploded, you caused " + _totalHit + " fooples in damage to other ships, and " + _damage + " to your own.")
	    	}
	    }
  }
  
  def collidesWith(_body:Body) = {
    
	  // a star collision will cause the torp to explode
	  if (_body.isInstanceOf[Star]) {
	    exploded
	  }
   
	  // a ship will take the entire blast
	  else if (_body.isInstanceOf[Ship]) {
		  val _s = _body.asInstanceOf[Ship]
		  if (this.ship.name != _s.name ) {
		    _s.torpHit(this)
		    hitShip = _s
		    _s.player ! ("msg", "Torp hit of "  + this.payload)
		    
		    // let player know that one of its torps hit another ship
		    val _thisShip = this.ship
		    if (_thisShip != null) {
		    	_thisShip.player ! ("msg","Torp hit of " + this.payload + " on ship " + _s.name )
		    }
		  }
	  }
  }
  
  // let the player know that a collision has occurred with the torp.
  def informPlayerCollision (_body:Body) = {
	  collidesWith(_body);
  }
  
  
  // when an object collides with a Torp, the torp explodes
  override def inelasticCollision (_body:Body) = { 
	informPlayerCollision (_body)
    // super.inelasticCollision(_body)
  }
  
  // when an object collides with a Torp, the torp explodes
  override def elasticCollision (_body:Body) = {
    informPlayerCollision (_body)
    // super.elasticCollision(_body)
  }

	def longMessage = s"${shortMessage} of energy ${this.payload}"

	def shortMessage = "torp"
}
