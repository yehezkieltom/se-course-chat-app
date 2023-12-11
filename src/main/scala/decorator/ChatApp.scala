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

  def broadcast(message: TextMessage): Unit

  def sendMessage(
    client: ClientConnection[Message],
    message: Message
  ): Unit

  def authenticate(message: AuthenticationMessage): Unit

  def getServerId(): Int

  def getClients(): Map[Int, ClientConnection[Message]]

  def addClient(clientConnection: ClientConnection[Message]): Unit

  def printLog(message: String): Unit

}

abstract class ChatServerDecorator(private val decorated: ChatServer)
  extends ChatServer {
    val serverId: Int = decorated.serverId


    def getServerId(): Int = {
      serverId
    }

    def getClients(): Map[Int, ClientConnection[Message]] = {
      decorated.getClients()
    }

    def addClient(clientConnection: ClientConnection[Message]): Unit = {
      decorated.addClient(clientConnection)
    }

    def isAuthenticationConfigured(): Unit = {
      throw ConfigurationError()
    }

    override def printLog(message: String): Unit = {
      decorated.printLog(message)
    }    
  }

class ChatServerBase(val sId: Int) extends ChatServer {
  override val serverId: Int = sId
  override val logger: Logger = Logger()
  var clients: Map[Int, ClientConnection[Message]] = new HashMap()

  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    clients += (clientConnection.clientId -> clientConnection)
  }

  override def handleMessage(message: Message): Unit = {
    message match {
      case textMessage: TextMessage => 
        broadcast(textMessage)
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
    }
  }

  override def broadcast(message: TextMessage): Unit = {
    printLog(s"Broadcasting message from sender ${message.sender}")
    val sender: Int = message.sender
    for (connection <- clients.values) {
      connection.sendMessage(message)
    }
  }

  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
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
  
  override def addClient(clientConnection: ClientConnection[Message]): Unit = {
    clients += (clientConnection.clientId -> clientConnection)
  }

  override def printLog(message: String): Unit = {
    logger.log(message)
  }

}

case class EncryptingServer(
  private val parent: ChatServer,
  val encryptionMethod: EncryptionMethod
  ) extends ChatServerDecorator(parent) {
    val logger: Logger = parent.logger
  
    def encryptText(text: String): String = {
      encryptionMethod.encrypt(text)
    }

    def decryptText(text: String): String = {
      encryptionMethod.decrypt(text)
    }

    override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
      parent.acceptClientConnection(clientConnection)
    }
    override def authenticate(message: AuthenticationMessage): Unit = {
      parent.authenticate(message)
    }

    override def broadcast(message: TextMessage): Unit = {
      parent.broadcast(message)
    }

    override def handleMessage(message: Message): Unit = {
      message match {
        case authMessage: AuthenticationMessage => 
          authenticate(
            AuthenticationMessage(
              authMessage.sender,
              decryptText(authMessage.username),
              decryptText(authMessage.password)
            )
          )
        case textMessage: TextMessageBase =>
          broadcast(
            TextMessageBase(
              textMessage.sender,
              decryptText(textMessage.message)
            )
          )
        case colTextMessage: ColoredTextMessage => 
          broadcast(
            ColoredTextMessage(
              TextMessageBase(
                colTextMessage._1.sender,
                decryptText(colTextMessage._1.message)
              ),
              colTextMessage.color
            )
          )
      }
    }
    override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
      val encryptedMessage = message match {
        case AuthenticationMessage(sender, username, password) =>
          AuthenticationMessage(sender, encryptText(username), encryptText(password))
        case TextMessageBase(sender, text) =>
          TextMessageBase(sender, encryptText(text))
        case ColoredTextMessage(textMessage, color) =>
          ColoredTextMessage(TextMessageBase(textMessage.sender, encryptText(textMessage.message)), color)
      }
      client.sendMessage(encryptedMessage)
    }

    // override def printLog(message: String): Unit = {
    //   parent.printLog(message)
    // }
}

case class AuthenticatingServer(
  private val parent: ChatServer, 
  val registeredUsers: Map[String, String]
  ) extends ChatServerDecorator(parent) {
    val logger: Logger = parent.logger
    var unauthenticatedClients: Map[Int, ClientConnection[Message]] = new HashMap()

    override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
      unauthenticatedClients += (clientConnection.clientId -> clientConnection)
  }

    override def authenticate(message: AuthenticationMessage): Unit = {
      val sender: Int = message.sender
      val username: String = message.username
      val password: String = message.password

      if (registeredUsers.get(username).contains(password)) {
        printLog(s"Successfully authenticated client: $sender")
        val connection = unauthenticatedClients(sender)
        unauthenticatedClients -= sender
        parent.addClient(connection)
      } else {
        printLog(s"Failed to authenticate client: $sender")
        sendMessage(
          unauthenticatedClients(sender),
          TextMessageBase(getServerId(), "Authentication failed.")
        )
      }
    }

    def isAuthenticated(clientId: Int): Boolean = {
      parent.getClients().keySet.contains(clientId)
    }

    override def broadcast(message: TextMessage): Unit = {
      val sender = message.sender
      if (!isAuthenticated(sender)) {
        printLog(s"Rejected message from unauthenticated client: $sender")
        sendMessage(
          unauthenticatedClients(sender),
          TextMessageBase(parent.serverId, "You must authenticate before sending messages.")
        )
        return
      }
      logger.log(s"Broadcasting message from sender ${message.sender}")
      for (connection <- parent.getClients().values) {
        connection.sendMessage(message)
      }
    }

    override def handleMessage(message: Message): Unit = {
      message match {
      case authMessage: AuthenticationMessage => 
        authenticate(authMessage)
      case textMessage: TextMessageBase => 
        broadcast(textMessage)
      }
    }

    override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
      client.sendMessage(message)
    }

    // override def printLog(message: String): Unit = {
    //   parent.printLog(message)
    // }
}

case class LoggingServer(private val parent: ChatServer) extends ChatServerDecorator(parent) {
  val logger: Logger = parent.logger

  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    printLog(s"New client: ${clientConnection.clientId}")
    parent.acceptClientConnection(clientConnection)
  }
  override def printLog(message: String): Unit = {
    logger.log(message)
  }

  override def authenticate(message: AuthenticationMessage): Unit = {
    val previousClients = parent.getClients()
    parent.authenticate(message)
    val currentClients = parent.getClients()

    // if (previousClients.size < currentClients.size) {
    //   printLog(s"Successfully authenticated client: ${message.sender}")
    // } else {
    //   printLog(s"Failed to authenticate client: ${message.sender}")
    // }
  }

  override def broadcast(message: TextMessage): Unit = {
    // if(!parent.getClients().contains(message.sender)) {
    //     printLog(s"Rejected message from unauthenticated client: ${message.sender}")
    //   } else {
    //     printLog(s"Broadcasting message from sender ${message.sender}")
    //   }
    parent.broadcast(message)
  }

  override def handleMessage(message: Message): Unit = {
    parent.handleMessage(message)
  }

  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
    client.sendMessage(message)
  }
  
}

//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val view: View
  val logger: Logger
  val serverConnection: ServerConnection[Message]
  var isAuthenticated: Boolean

  override def handleMessage(message: Message): Unit

  def send(message: String): Unit
  def send(message: String, color: String): Unit
  def authenticate(username: String, password: String): Unit
  def encryptText(message: String): String
  def decryptText(message: String): String
}

class ChatClientBase(
  val cId: Int,
  val serverId: Int,
  networkSimulator: NetworkSimulator[Message]
) extends ChatClient {
  val clientId: Int = cId
  val logger: Logger = Logger()
  val view: View = View()

  override val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))

  var isAuthenticated = false

  override def authenticate(username: String, password: String): Unit = {
    throw ConfigurationError()
  }
    
  override def handleMessage(message: Message): Unit = {

    message match {
      case textMessage: TextMessageBase =>
        displayMessage(textMessage)
      case _ => 
        throw ConfigurationError()
    }
  }

  private def displayMessage(message: TextMessageBase): Unit = {
    view.printMessage(message.sender, message.message)
  }

  override def send(message: String): Unit = {
    var textMessage: TextMessageBase = TextMessageBase(clientId, message)
    serverConnection.sendMessage(textMessage)
  }

  override def send(message: String, color: String): Unit = {
    throw ConfigurationError()
  }

  override def encryptText(message: String): String = message

  override def decryptText(message: String): String = message
}

abstract class ChatClientDecorator(private val decorated: ChatClient) 
  extends ChatClient {
    val serverId: Int = decorated.clientId
    val logger: Logger = decorated.logger
    val view: View = decorated.view
    val serverConnection = decorated.serverConnection
    var isAuthenticated = decorated.isAuthenticated
    
    def getServerId(): Int = serverId

    def encryptText(message: String): String = message

    def decryptText(message: String): String = message
}

case class ColoringClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {
  val clientId: Int = parent.clientId

  override def authenticate(username: String, password: String): Unit = {
    parent.authenticate(username, password)
  }

  override def handleMessage(message: Message): Unit = {
    message match {
      case coloredTextMessage: ColoredTextMessage =>
        displayColoredMessage(coloredTextMessage)
      case _ =>
        parent.handleMessage(message)
    }
  }

  private def displayColoredMessage(message: ColoredTextMessage): Unit = {
    view.printMessage(message._1.sender, message._1.message, message.color)
  }

  override def send(message: String): Unit = {
    parent.send(encryptText(message))
  }

  override def send(message: String, color: String): Unit = {
    val text = encryptText(message)
    val coloredTextMessage = ColoredTextMessage(TextMessageBase(clientId, text), color)
    serverConnection.sendMessage(coloredTextMessage)
  }

  override def encryptText(message: String): String = parent.encryptText(message)

  override def decryptText(message: String): String = parent.decryptText(message)

    
}

case class EncryptingClient(
  private val parent: ChatClient, 
  private val encryptionMethod: EncryptionMethod) 
  extends ChatClientDecorator(parent) {
    val clientId: Int = parent.clientId

    override def decryptText(text: String): String = {
      encryptionMethod.decrypt(text)
    }

    override def encryptText(text: String): String = {
      encryptionMethod.encrypt(text)
    }

    override def authenticate(username: String, password: String): Unit = {
      parent.authenticate(username, password)
    }

    override def handleMessage(message: Message): Unit = {
      val decryptedMessage: Message = 
        message match {
          case authMessage: AuthenticationMessage =>
            AuthenticationMessage(
              authMessage.sender,
              decryptText(authMessage.username),
              decryptText(authMessage.password)
            )
          case textMessage: TextMessageBase =>
            TextMessageBase(textMessage.sender, decryptText(textMessage.message))
          case coloredTextMessage: ColoredTextMessage =>
            ColoredTextMessage(
              TextMessageBase(
                coloredTextMessage._1.sender,
                decryptText(coloredTextMessage._1.message)
              ),
              coloredTextMessage.color
            )
        }
      parent.handleMessage(decryptedMessage)
    }

    override def send(message: String): Unit = {
      parent.send(message)
    }

    override def send(message: String, color: String): Unit = {
      parent.send(message, color)
    }
}

case class AuthenticatingClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {
  val clientId: Int = parent.clientId
  
  override def authenticate(username: String, password: String): Unit = {
    if (!parent.isAuthenticated) {
      val message = AuthenticationMessage(
        parent.clientId, 
        encryptText(username), 
        encryptText(password))
      //logger need to show unencrypted username and password so you cannot show actual username and password,
      //also other log for show sent message may need the unencrypted version of the message
      parent.serverConnection.sendMessage(message)
    }
  }

  override def encryptText(message: String): String = {
    parent.encryptText(message)
  }

  override def decryptText(message: String): String = {
    parent.decryptText(message)
  }

  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage.username, authMessage.password)
      case _ => 
        parent.handleMessage(message)
    }
  }

  override def send(message: String): Unit = {
    parent.send(message)
  }

  override def send(message: String, color: String): Unit = {
    parent.send(message, color)
  }
}

case class LoggingClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {
  val clientId: Int = parent.clientId

  override def handleMessage(message: Message): Unit = {
    logger.log(s"Received message from sender ${message.sender}")
    parent.handleMessage(message: Message)
  }
  override def send(message: String): Unit = {
    logger.log(s"Sending message: ${TextMessageBase(clientId, message)}")
    parent.send(message)
  }
  override def send(message: String, color: String): Unit = {
    logger.log(s"Sending message: ${
      ColoredTextMessage(TextMessageBase(clientId, message), color)
    }")
    parent.send(message, color)
  }
  override def authenticate(username: String, password: String): Unit = {
    logger.log(s"Sending authentication request: ${AuthenticationMessage(clientId, username, password)}")
    parent.authenticate(username, password)
  }
}
