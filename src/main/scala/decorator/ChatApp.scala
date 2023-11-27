package de.uni_saarland.cs.se
package decorator

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

case class TextMessageBase(
    override val sender: Int,
    override val message: String
) extends TextMessage(message),
      Message(sender)

abstract class TextMessageDecorator(private val decorated: TextMessage)
    extends TextMessage(decorated.message),
      Message(decorated.sender)

case class ColoredTextMessage(
    private val parent: TextMessage,
    color: String
) extends TextMessageDecorator(parent) {
  override def toString: String = parent.toString
}

//==============================================================================
// Server
//==============================================================================

trait ChatServer() extends Server[Message] {
  val serverId: Int
  val logger: View

  override def handleMessage(message: Message): Unit

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit
}

//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val view: View
  val logger: View

  override def handleMessage(message: Message): Unit

  def send(message: String): Unit
  def send(message: String, color: String): Unit
  def authenticate(username: String, password: String): Unit
}
