package de.uni_saarland.cs.se
package decorator

import util.*

import scala.collection.immutable.HashMap
import de.uni_saarland.cs.se.util.REVERSE.encrypt
import org.w3c.dom.Text

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

  def addClient(clientConnection: ClientConnection[Message]): Unit

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

    override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
      printLog(s"New client: ${clientConnection.clientId}")
      addClient(clientConnection)
    }

    def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
      val toBeSentMessage = message match {
        case authMessage: AuthenticationMessage =>
          AuthenticationMessage(
            authMessage.sender,
            authMessage.username,
            authMessage.password
          )
        case textMessage: TextMessageBase =>
          TextMessageBase(textMessage.sender, textMessage.message)
      }
      client.sendMessage(toBeSentMessage)
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
  
  override def addClient(clientConnection: ClientConnection[Message]): Unit = {
    clients += (clientConnection.clientId -> clientConnection)
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

    override def authenticate(message: AuthenticationMessage): Unit = {
      //TODO decrypt the username and password in message before passing to authenticate
      parent.authenticate(message)
    }

    override def broadcast(message: TextMessageBase): Unit = {
      parent.broadcast(message)
    }

    override def handleMessage(message: Message): Unit = {
      message match {
        case authMessage: AuthenticationMessage => 
          parent.handleMessage(
            AuthenticationMessage(
              authMessage.sender,
              encryptText(authMessage.username),
              encryptText(authMessage.password)
            )
          )
        case textMessage: TextMessageBase =>
          parent.handleMessage(
            TextMessageBase(
              textMessage.sender,
              encryptText(textMessage.message)
            )
          )
      }
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

      for (connection <- parent.getClients().values) {
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
  val serverConnection: ServerConnection[Message]
  var isAuthenticated: Boolean

  override def handleMessage(message: Message): Unit

  def send(message: String): Unit
  def send(message: String, color: String): Unit
  def authenticate(username: String, password: String): Unit
  def printLog(message: String): Unit
}

class ChatClientBase(
  val cId: Int,
  val serverId: Int,
  networkSimulator: NetworkSimulator[Message]
) extends ChatClient {
  override val clientId: Int = cId
  override val logger: Logger = Logger()
  override val view: View = View()

  override val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))

  override var isAuthenticated = false

  override def authenticate(username: String, password: String): Unit = {
    throw ConfigurationError()
  }
    
  override def handleMessage(message: Message): Unit = {
    printLog(s"Received message from sender ${message.sender}")

    message match {
      case authMessage: AuthenticationMessage => 
        throw ConfigurationError()
      case textMessage: TextMessageBase =>
        displayMessage(textMessage)
      case coloredTextMessage: ColoredTextMessage =>
        throw ConfigurationError()
    }
  }

  private def displayMessage(message: TextMessageBase): Unit = {
    view.printMessage(message.sender, message.message)
  }

  override def send(message: String): Unit = {
    var textMessage: TextMessageBase = TextMessageBase(clientId, message)
    printLog(s"Sending message: ${textMessage}")
    serverConnection.sendMessage(textMessage)
  }

  override def send(message: String, color: String): Unit = {
    throw ConfigurationError()
  }

  override def printLog(message: String): Unit = {
    return
  }
}

abstract class ChatClientDecorator(private val decorated: ChatClient) 
  extends ChatClient {
    val serverId: Int = decorated.clientId
    val logger: Logger = decorated.logger
    val view: View = decorated.view
    val serverConnection = decorated.serverConnection
    var isAuthenticated = decorated.isAuthenticated
    
    def getServerId(): Int = serverId
}

case class ColoringClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {

  override def authenticate(username: String, password: String): Unit = {
    parent.authenticate(username, password)
  }

  override def handleMessage(message: Message): Unit = {
    printLog(s"Received message from sender ${message.sender}")

    message match {
      case authMessage: AuthenticationMessage => 
        throw ConfigurationError()
      case textMessage: TextMessageBase =>
        displayMessage(textMessage)
      case coloredTextMessage: ColoredTextMessage =>
        displayColoredMessage(coloredTextMessage)
    }
  }

  private def displayMessage(message: TextMessageBase): Unit = {
    view.printMessage(message.sender, message.message)
  }

  private def displayColoredMessage(message: ColoredTextMessage): Unit = {
    view.printMessage(message._1.sender, message._1.message, message.color)
  }

  override def printLog(message: String): Unit = {
    parent.printLog(message)
  }

  override def send(message: String): Unit = {
    parent.send(message)
  }

  override def send(message: String, color: String): Unit = {
    parent.send(message, color)
  }
    
}

case class EncryptingClient(
  private val parent: ChatClient, 
  private val encryptionMethod: EncryptionMethod) 
  extends ChatClientDecorator(parent) {

    private def decryptText(text: String): String = {
      encryptionMethod.decrypt(text)
    }

    private def encryptText(text: String): String = {
      encryptionMethod.encrypt(text)
    }

    override def authenticate(username: String, password: String): Unit = {
      parent.authenticate(encryptText(username), decryptText(password))
    }

    override def handleMessage(message: Message): Unit = {
      parent.handleMessage(message)
    }

    override def printLog(message: String): Unit = {
      parent.printLog(message)
    }

    override def send(message: String): Unit = {
      parent.send(message)
    }

    override def send(message: String, color: String): Unit = {
      parent.send(message, color)
    }
}

case class AuthenticatingClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {
  override def authenticate(username: String, password: String): Unit = {
    if (!parent.isAuthenticated) {
      val message = AuthenticationMessage(parent.clientId, username, password)
      //logger need to show unencrypted username and password so you cannot show actual username and password,
      //also other log for show sent message may need the unencrypted version of the message
      printLog(s"Sending authentication requestL ${}")
  }
}

case class LoggingClient(private val parent: ChatClient) extends ChatClientDecorator(parent) {

}
