package org.costine.codacas

import fi.utu.cs.physics.Body
import java.util.Date

import org.costine.codacas.messages.ExplodedMessage
import org.costine.codacas.messages.TextMessage
class Torp(val payload:Int, var fuel:Long, val range:Double, val ship:Ship,
					 m:Double, s:Double, loc:Coordinate, u:Universe)
	extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe){
  
  // when the torp hits a ship, set this so that the explosion doesn't affect the
  // hit ship - it already received damage from the payload
  var hitShip: Option[Ship] = None
  
  var lastRecharge: Long = new Date().getTime
  
  // updates the torp after it is launched - in this case a "recharge" means to
  // remove "fuel". "fuel" is the number of ms that the torp will last.
  // when it gets to 0 ships players within range will be notified 
  def recharge(): Unit = {

	  Some(new Date().getTime - lastRecharge).foreach { d => if (d > 0) fuel -= d}
   
	  // ran out of fuel - time to detonate, which will remove this torp from the universe.
	  if (fuel <= 0) exploded()
		else lastRecharge = new Date().getTime
  }
  
  def exploded(): Unit = {

		val validHits = universe.ships()
			.filter { aShip =>
				!aShip.yanked &&
					aShip.isAlive &&
					(hitShip.isEmpty || hitShip.exists { _ != aShip})
			}
			.map { aShip =>(aShip,aShip.torpExploded(this))}
			.filter { case (_, hit) => hit > 0}

		validHits.foreach	{ case (aShip,hit) => aShip.player ! ExplodedMessage(this, hit)}

		val totalHit =
			validHits
				.filter { case (aShip,_) => aShip != ship}
				.map { case (_, hit) => hit }.sum

		val damage =
			validHits
				.filter { case (aShip,_) => aShip == ship}
				.map { case (_, hit) => hit }.sum

		val totalDamage = totalHit+damage
		(if ( (totalDamage <= 0) && hitShip.isEmpty)
			Some("Torp Missed")
		else if (totalDamage > 0)
			Some (s"Torp exploded, you caused $totalHit fooples in damage to other ships, and $damage to your own.")
		else None).foreach { msg =>
			ship.player ! TextMessage(msg)
		}


		// this torp goes away now
		this.setVelocity(0.0,0.0)
		universe.remove(this)
		hitShip = None

  }
  
  def collidesWith(body:Body): Unit = {
    body match {
			case star: Star => exploded()
			case thatShip: Ship =>
				if (this.ship.name != thatShip.name ) {
					thatShip.torpHit(this)
					hitShip = Some(thatShip)
					thatShip.player ! TextMessage(s"Torp hit of $payload")

					// let player know that one of its torps hit another ship
					Option(this.ship).foreach { thisShip =>
						thisShip.player ! TextMessage(s"Torp hit of $payload on ship ${thatShip.name}" )
					}
				}
		}


  }
  
  // let the player know that a collision has occurred with the torp.
  def informPlayerCollision (body:Body): Unit = collidesWith(body)

  // when an object collides with a Torp, the torp explodes
  override def inelasticCollision (body:Body): Unit = informPlayerCollision (body)
  
  // when an object collides with a Torp, the torp explodes
  override def elasticCollision (body:Body): Unit = informPlayerCollision (body)


	def longMessage = s"$shortMessage of energy $payload"

	def shortMessage = "torp"
}
