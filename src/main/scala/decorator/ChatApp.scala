package de.uni_saarland.cs.se
package decorator

import util.*

import scala.collection.immutable.HashMap

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
  val logger: Logger

  override def handleMessage(message: Message): Unit

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit

  def broadcast(message: TextMessageBase): Unit

  def sendMessage(
    client: ClientConnection[Message],
    messaage: Message
  ): Unit

  def authenticate(message: AuthenticationMessage): Unit

  def getServerId(): Int

  def getClients(): Map[Int, ClientConnection[Message]]

}

abstract class ChatServerDecorator(private val decorated: ChatServer)
  extends ChatServer {
    val serverId: Int = decorated.serverId
    var clients: Map[Int, ClientConnection[Message]] = new HashMap()


    def getServerId(): Int = {
      serverId
    }

    def getClients(): Map[Int, ClientConnection[Message]] = {
      clients
    }

    def addClient(clientConnection: ClientConnection[Message]): Unit = {
      clients += (clientConnection.clientId -> clientConnection)
    }

    def isAuthenticationConfigured(): Unit = {
      throw ConfigurationError()
    }

    def encryptText(text: String): String = {
      text
    }

    def decryptText(text: String): String = {
      text
    }

    override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
      printLog(s"New client: ${clientConnection.clientId}")
      addClient(clientConnection)
    }

    def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
      val encryptedMessage = message match {
        case AuthenticationMessage(sender, username, password) =>
          AuthenticationMessage(
            sender,
            encryptText(username),
            encryptText(password)
          )
        case TextMessageBase(sender, message) =>
          TextMessageBase(sender, encryptText(message))
        case ColoredTextMessage(message, color) =>
          ColoredTextMessage(TextMessageBase(message.sender, encryptText(message.message)), color)
      }
      client.sendMessage(encryptedMessage)
    }
    def printLog(message: String): Unit = {
      return
    }
    
  }

class ChatServerBase(val sId: Int) extends ChatServer {
  val serverId: Int = sId
  val logger: Logger = new Logger()
  var clients: Map[Int, ClientConnection[Message]] = new HashMap()

  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    clients += (clientConnection.clientId -> clientConnection)
  }

  override def handleMessage(message: Message): Unit = {
    message match {
      case textMessage: TextMessageBase => 
        broadcast(textMessage)
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
    }
  }

  override def broadcast(message: TextMessageBase): Unit = {
    val sender: Int = message.sender
    for (connection <- clients.values) {
      connection.sendMessage(message)
    }
  }

  override def sendMessage(
    client: ClientConnection[Message],
    message: Message
    ): Unit = {
      client.sendMessage(message)
  }

  override def authenticate(message: AuthenticationMessage): Unit = {
    throw ConfigurationError()
  }

  override def getServerId(): Int = {
    serverId
  }

  override def getClients(): Map[Int, ClientConnection[Message]] = {
    clients
  }

}

case class EncryptingServer(
  private val parent: ChatServer,
  val encryptionMethod: EncryptionMethod
  ) extends ChatServerDecorator(parent) {
    val logger: Logger = parent.logger
  
    override def encryptText(text: String): String = {
      encryptionMethod.encrypt(text)
    }

    override def decryptText(text: String): String = {
      encryptionMethod.decrypt(text)
    }

    override def authenticate(message: AuthenticationMessage): Unit = {
      parent.authenticate(message)
    }

    override def broadcast(message: TextMessageBase): Unit = {
      parent.broadcast(message)
    }

    override def handleMessage(message: Message): Unit = {
      parent.handleMessage(message)
    }
}

case class AuthenticatingServer(
  private val parent: ChatServer, 
  val registeredUsers: Map[String, String]
  ) extends ChatServerDecorator(parent) {
    val logger: Logger = parent.logger
    var unauthenticatedClients: Map[Int, ClientConnection[Message]] = new HashMap()

    override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
      printLog(s"New client: ${clientConnection.clientId}")
      unauthenticatedClients += (clientConnection.clientId -> clientConnection)
  }

    override def authenticate(message: AuthenticationMessage): Unit = {
      val sender: Int = message.sender
      val username: String = decryptText(message.username)
      val password: String = decryptText(message.password)

      if (registeredUsers.get(username).contains(password)) {
        printLog(s"Successfully authenticated client: $sender")
        val connection = unauthenticatedClients(sender)
        unauthenticatedClients -= sender
        addClient(connection)
      } else {
        printLog(s"Failed to authenticate client: $sender")
        sendMessage(
          unauthenticatedClients(sender),
          TextMessageBase(getServerId(), "Authentication failed.")
        )
      }
    }

    def isAuthenticated(clientId: Int): Boolean = {
      getClients().keySet.contains(clientId)
    }

    override def broadcast(message: TextMessageBase): Unit = {
      val sender = message.sender
      if (!isAuthenticated(sender)) {
        printLog(s"Rejected message from unauthenticated client: ${message.sender}")
        sendMessage(
          unauthenticatedClients(sender),
          TextMessageBase(getServerId(), "You must authenticate before sending messages.")
        )
        return
      }
      printLog(s"Broadcasting message from sender ${sender}")

      for (connection <- getClients().values) {
        connection.sendMessage(message)
      }
    }

    override def handleMessage(message: Message): Unit = {
      parent.handleMessage(message)
    }
}

case class LoggingServer(private val parent: ChatServer) extends ChatServerDecorator(parent) {
  val logger: Logger = parent.logger
  override def printLog(message: String): Unit = {
    logger.log(message)
  }

  override def authenticate(message: AuthenticationMessage): Unit = {
    parent.authenticate(message)
  }

  override def broadcast(message: TextMessageBase): Unit = {
    parent.broadcast(message)
  }

  override def handleMessage(message: Message): Unit = {
    parent.handleMessage(message)
  }
  
}

//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val view: View
  val logger: Logger

  override def handleMessage(message: Message): Unit

  def send(message: String): Unit
  def send(message: String, color: String): Unit
  def authenticate(username: String, password: String): Unit
}

class ChatClientDecorator() extends ChatClient {

}

class ChatClientBase() extends ChatClient {

}

class ColoringClient() extends ChatClientDecorator{
  
}

class EncryptingClient() extends ChatClientDecorator {

}

class AuthenticatingClient() extends ChatClientDecorator {

}

class LoggingClient() extends ChatClientDecorator {

}
