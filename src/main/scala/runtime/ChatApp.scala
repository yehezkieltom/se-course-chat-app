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
  val logger: Logger = Logger()

  private var clients: Map[Int, ClientConnection[Message]] = new HashMap()
  private var unauthenticatedClients: Map[Int, ClientConnection[Message]] =
    new HashMap()

  override def handleMessage(message: Message): Unit = {
    // TODO
    message match {
      case authMessage: AuthenticationMessage => 
        authenticate(authMessage)
      case textMessage: TextMessage => 
        broadcast(textMessage)
    }
    
  }

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit = {
    if (config.logging) {
      logger.log(s"New client: ${clientConnection.clientId}")
    }
    if (config.authentication) {
      unauthenticatedClients += (clientConnection.clientId -> clientConnection)
    } else {
      clients += (clientConnection.clientId -> clientConnection)
    }
  }

  private def authenticate(message: AuthenticationMessage): Unit = {
    if (!config.authentication) {
          throw ConfigurationError()
    }
    val sender: Int = message.sender
    var username: String = message.username
    var password: String = message.password
    if (config.encryption.isDefined) {
      username = config.encryption.get.decrypt(username)
      password = config.encryption.get.decrypt(password)
    }
        
    if (registeredUsers.get(username).contains(password)) {
      if config.logging then logger.log(s"Successfully authenticated client: $sender")
      val connection = unauthenticatedClients(sender)
      unauthenticatedClients - sender
      clients += (sender, connection)
    } else {
      if config.logging then logger.log(s"Failed to authenticate client: $sender")
      sendMessage(
        unauthenticatedClients(sender),
        TextMessage(serverId, "Authentication failed.")
      )
    }
  }

  private def broadcast(message: TextMessage): Unit = {
    val sender = message.sender
    if (config.authentication) {
      if (!isAuthenticated(sender)) {
        if (config.logging) {
          logger.log(s"Rejected message from unauthenticated client: ${message.sender}")
        }
        sendMessage(
          unauthenticatedClients(sender),
          TextMessage(serverId, "You must authenticate before sending messages.")
        )
        return
      }
    }
    if (config.logging) {
      logger.log(s"Broadcasting message from sender ${message.sender}")
    }

    for (connection <- clients.values) {
      connection.sendMessage(message)
    }

  }

  private def isAuthenticated(clientId: Int): Boolean = {
    clients.keySet.contains(clientId)
  }

  private def sendMessage(
    client: ClientConnection[Message],
    message: Message
    ): Unit = {
      if (config.encryption.isDefined) {
        val encryptedMessage = message match {
          case AuthenticationMessage(sender, username, password) => 
            AuthenticationMessage(
              sender,
              config.encryption.get.encrypt(username),
              config.encryption.get.encrypt(password)
            )
          case TextMessage(sender, message, color) => 
            TextMessage(sender, config.encryption.get.encrypt(message), color)
        }
        client.sendMessage(encryptedMessage)
      } else {
        client.sendMessage(message)
      }
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

  private val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))
  private var isAuthenticated = false

  override def handleMessage(message: Message): Unit = {
    if (config.logging) {
      logger.log(s"Received message from sender ${message.sender}")
    }
    
    message match {
      case authMessage: AuthenticationMessage =>
        if (config.authentication) {
          if (authMessage.sender == serverId) {
            isAuthenticated = true
          }
        } else {
          throw ConfigurationError()
        }
      case textMessage: TextMessage => 
        displayMessage(textMessage)
    }
  }

  private def displayMessage(message: TextMessage): Unit = {
    var text: String = message.message
    if (config.encryption.isDefined) {
      text = config.encryption.get.decrypt(text)
    }
    if (config.color) {
      view.printMessage(message.sender, text, message.color)
    } else {
      view.printMessage(message.sender, text)
    }
  }
  def send(message: String, color: String = BLACK): Unit = {
    // TODO
    var text: String = message
    if (config.encryption.isDefined) {
      text = config.encryption.get.encrypt(text)
    }

    var textMessage: TextMessage = TextMessage(clientId, text)
    if (config.color) {
      textMessage = TextMessage(clientId, text, color)
    }
    if (config.logging) {
      logger.log(s"Sending message: ${
        TextMessage(clientId, message)
      }")
    }

    serverConnection.sendMessage(textMessage)
  }

  def authenticate(username: String, password: String): Unit = {
    // TODO
    if (!config.authentication) {
      throw ConfigurationError()
    }

    if (!isAuthenticated) {
      var message: AuthenticationMessage = AuthenticationMessage(clientId, username, password)
      if (config.encryption.isDefined) {
        message = AuthenticationMessage(
          clientId, 
          config.encryption.get.encrypt(username),
          config.encryption.get.encrypt(password)
        )
      }

      if (config.logging) {
        logger.log(
          s"Sending authentication request: ${AuthenticationMessage(clientId, username, password)}"
          )
      }
      serverConnection.sendMessage(message)
    }
  }
}
