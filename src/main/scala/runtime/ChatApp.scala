package de.uni_saarland.cs.se
package runtime

import util.*

import scala.Console.BLACK
import scala.collection.immutable.HashMap

class ChatConfig(
    val authentication: Boolean,
    val color: Boolean,
    val encryption: Option[EncryptionMethod],
    val logging: Boolean
) {}

//==============================================================================
// Messages
//==============================================================================
sealed trait Message(val sender: Int)
case class AuthenticationMessage(
    override val sender: Int,
    username: String,
    password: String
) extends Message(sender) {
  override def toString: String = s"[$sender] u=$username p=$password"
}

case class TextMessage(
    override val sender: Int,
    message: String,
    color: String = Console.BLACK
) extends Message(sender) {
  override def toString: String = s"[$sender] $message"
}

//==============================================================================
// Server
//==============================================================================
class ChatServer(
    val config: ChatConfig,
    val serverId: Int,
    private val registeredUsers: Map[String, String] = HashMap()
) extends Server[Message] {
  val logger: Logger                                       = Logger()

  override def handleMessage(message: Message): Unit = {
    // TODO
  }

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit = {
    // TODO
  }
}

//==============================================================================
// Client
//==============================================================================
class ChatClient(
    val config: ChatConfig,
    val clientId: Int,
    val serverId: Int,
    networkSimulator: NetworkSimulator[Message]
) extends Client[Message] {
  val view: View     = View()
  val logger: Logger = Logger()

  override def handleMessage(message: Message): Unit = {
    // TODO
  }

  def send(message: String, color: String = BLACK): Unit = {
    // TODO
  }

  def authenticate(username: String, password: String): Unit = {
    // TODO
  }
}
