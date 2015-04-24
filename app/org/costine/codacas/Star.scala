package org.costine.codacas

import org.costine.codacas.actors.Player
import fi.utu.cs.physics.Body
class Star(rr:Double, mw:Double, m:Double, s:Double, loc:Coordinate, u:Universe)
  extends MassiveObject (m:Double, s: Double, loc:Coordinate, u:Universe) {

  def maxWql:Double = mw;
  var wqlCapacity:Double = maxWql / 2;
  def wql:Double = wqlCapacity
  
  def starRechargeRate = rr;

  def drain(req:Double):Double = {
    if (req > wql) {
      val retWql = wqlCapacity
      wqlCapacity = 0
      retWql
    }
    else {
      wqlCapacity = wqlCapacity - req
      req
    }
  }
  
  def recharge(amt:Double):Double = {
    if (amt + wql > maxWql) {
      wqlCapacity=maxWql
    }
    else {
      wqlCapacity=amt+wql
    }
    wql
  }

  def recharge() = {
    recharge(starRechargeRate)
  }
  
  // let the body know that a collision has occurred.
  def informPlayerCollision (_body:Body) = {
	  if (_body.isInstanceOf[Ship]) {
		  _body.asInstanceOf[Ship].player ! ("collide",this)
	  }
	  else if (_body.isInstanceOf[Torp]) {
		  _body.asInstanceOf[Torp].collidesWith(this.asInstanceOf[Body])
	  }
  }
  
  override def inelasticCollision (_body:Body) = { 
	informPlayerCollision (_body)
    super.inelasticCollision(_body)
  }
  
  override def elasticCollision (_body:Body) = {
    informPlayerCollision (_body)
    super.elasticCollision(_body)
  }

  def longMessage = s"${shortMessage} of radius ${getRadius}"

  def shortMessage = "star"
}