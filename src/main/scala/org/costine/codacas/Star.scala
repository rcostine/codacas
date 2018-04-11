package org.costine.codacas
import fi.utu.cs.physics.Body
import org.costine.codacas.messages.CollisionMessage

class Star(rr:Double, mw:Double, m:Double, s:Double, loc:Coordinate, u:Universe)
  extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe) {

  var wqlCapacity: Double = maxWql / 2

  def maxWql: Double = mw

  def wql: Double = wqlCapacity

  def starRechargeRate: Double = rr

  def drain(req: Double): Double =
    if (req > wql) {
      val retWql = wqlCapacity
      wqlCapacity = 0
      retWql
    }
    else {
      wqlCapacity = wqlCapacity - req
      req
    }

  def recharge(amt: Double): Double = {
    wqlCapacity = if (amt + wql > maxWql) maxWql else amt + wql
    wqlCapacity
  }

  def recharge(): Unit = recharge(starRechargeRate)

  // let the body know that a collision has occurred.
  def informPlayerCollision(body: Body): Unit = body match {
    case ship: Ship => ship.player ! CollisionMessage(this)
    case torp: Torp => torp.collidesWith(this)
  }

  override def inelasticCollision(_body: Body): Unit = {
    informPlayerCollision(_body)
    super.inelasticCollision(_body)
  }

  override def elasticCollision(_body: Body): Unit = {
    informPlayerCollision(_body)
    super.elasticCollision(_body)
  }

  def longMessage = s"$shortMessage of radius $getRadius"

  def shortMessage = "star"

  override def toString : String = List(
    s"Mass: ${Option(getMass).map(d => d.toString).getOrElse("None")}",
    s"Radius: ${Option(getRadius).map(d => d.toString).getOrElse("None")}",
    s"Density: ${density.map(d => d.toString).getOrElse("None")}",
    s"Pos(x,y): ${Option(getPosition).map(d => d.toString).getOrElse("None")}"
  ).mkString("; ")

}