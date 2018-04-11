package org.costine.codacas

import akka.actor.ActorSystem

package object actors {
  implicit val actorSystem = ActorSystem("codacasng")
}
