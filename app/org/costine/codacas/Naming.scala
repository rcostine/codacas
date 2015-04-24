package org.costine.codacas

/**
 * Created by rjc on 4/20/15.
 */
trait Naming {
  def thisCollidingMessage(_other: Naming) : String = {
    s"${this.longMessage} ${collidingMessage(_other)}"
  }

  def collidingMessage(_other: Naming) : String = {
    s"collided with ${_other.longMessage}"
  }
  def longMessage: String

  def shortMessage: String
}
