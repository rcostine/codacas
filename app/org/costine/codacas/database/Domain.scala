package org.costine.codacas.database

import akka.actor.ActorRef

case class User (name: String, password:String)

case class Stat (user: User, key:String, value:String)

case class UserValidationRequest(_user:User, _actor:ActorRef, _validated:String)

case class UserValidationResponse(_user:User, _validated:String)
