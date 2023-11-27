package de.uni_saarland.cs.se
package traits

import util.*

//==============================================================================
// Messages
//==============================================================================
trait Message(val sender: Int)
case class AuthenticationMessage(
    override val sender: Int,
    username: String,
    password: String
) extends Message(sender) {
  override def toString: String = s"[$sender] u=$username p=$password"
}

trait TextMessage(val message: String) extends Message {
  override def toString: String = s"[$sender] $message"
}

trait Colored(val color: String)

case class PlainTextMessage(
    override val sender: Int,
    override val message: String
) extends TextMessage(message),
      Message(sender)

case class ColoredTextMessage(
    override val sender: Int,
    override val message: String,
    override val color: String
) extends TextMessage(message),
      Message(sender),
      Colored(color)

//==============================================================================
// Server
//==============================================================================

trait ChatServer() extends Server[Message] {
  val serverId: Int

  override def handleMessage(message: Message): Unit

  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit
}

//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val serverId: Int
  val view: View

  override def handleMessage(message: Message): Unit

  def send(message: String): Unit
}