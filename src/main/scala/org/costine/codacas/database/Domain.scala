package org.costine.codacas.database

import akka.actor.ActorRef

case class User (name: String, password:String)

case class Stat (user: User, key:String, value:String)

case class UserValidationRequest(user:User, actor:ActorRef, validated:String)

case class UserValidationResponse(user:User, validated:String)
