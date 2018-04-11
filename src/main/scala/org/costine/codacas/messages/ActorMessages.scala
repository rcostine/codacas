package org.costine.codacas.messages

import fi.utu.cs.physics.Body
import org.costine.codacas.Naming
import org.costine.codacas.Torp
import org.costine.codacas.actors.Player
import org.costine.codacas.database.User
import org.costine.codacas.network.Handler

// sent by Play (aka activator) to the game when the system is stopped.
// Game will forward to all Players, who should then shutdown their sockets
case class GameShutdownMessage(text: String, time: Long)

case class ShutdownMessage(text:String, time: Long)

case class SocketClosedMessage(exception:Exception)

case class PlayerOffMessage(cm: SocketClosedMessage, handler: Handler)

case class BroadcastMessage(text: String)

case class TextMessage(text: String)

case class BroadcastTextMessage(text: String)

case class CommandMessage(text: String)

case class PlayerOnMessage(player: Player)

case class CollisionMessage(body: Body)

case class NamedCollisionMessage(body: Naming, player: Player)

case class ExplodedMessage(body: Torp, damage: Int)

case class WhoMessage()

case class SetWarpMagnitudeMessage(magnitude: Double)

case class GetWarpMagnitudeMessage()

case class GameCharacterMessage(char: Char)

case class CharacterFromHandlerMessage(char: Char, handler: Handler)

case class PlayerRegistrationMessage(handler: Handler, shipName: String, commandMagnitudeFactor: Double)

case class PlayerHandlerRegistrationMessage(handler: Handler)

case class UserMessage(user: User)
