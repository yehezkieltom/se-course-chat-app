package de.uni_saarland.cs.se

import decorator.*
import util.{ConfigurationError, NetworkSimulator, REVERSE, ROT13, View}

import org.scalatest.flatspec.AnyFlatSpec

import scala.Console.RESET

class DecoratorChatAppTest extends AnyFlatSpec {
  def assertLastMessage(
      client: ChatClient,
      expectedSender: Int,
      expectedText: String
  ): Unit = {
    val actualMessage = client.view.lastDisplayedMessage()
    assert(actualMessage === s"[$expectedSender] $expectedText")
  }

  def assertLastMessage(
      client: ChatClient,
      expectedSender: Int,
      expectedText: String,
      expectedColor: String
  ): Unit = {
    val actualMessage = client.view.lastDisplayedMessage()
    assert(
      actualMessage === s"$RESET$expectedColor[$expectedSender] $expectedText$RESET"
    )
  }


//==============================================================================
// Decorator order
//==============================================================================
  "The decorators" must "be added in the correct order" in {
    val network = NetworkSimulator[Message]()
    val server = LoggingServer(
      AuthenticatingServer(
        EncryptingServer(ChatServerBase(0), ROT13),
        Map("user2" -> "securePassword")
      )
    )
    network.registerServer(server)
    val client = LoggingClient(
      AuthenticatingClient(
        EncryptingClient(
          ColoringClient(ChatClientBase(1, server.serverId, network)), ROT13
        )
      )
    )
    network.registerClient(client)
  }

//==============================================================================
// Basic tests
//==============================================================================
  "A Chat application" should "display sent messages" in {
    val network = NetworkSimulator[Message]()
    val server  = ChatServerBase(0)
    network.registerServer(server)
    val client1 = ChatClientBase(1, server.serverId, network)
    network.registerClient(client1)
    val client2 = ChatClientBase(2, server.serverId, network)
    network.registerClient(client2)

    client1.send("HelloWorld!")
    assertLastMessage(client1, client1.clientId, "HelloWorld!")
    assertLastMessage(client2, client1.clientId, "HelloWorld!")
  }

  // ==============================================================================
  // Authentication tests
  // ==============================================================================
  "A Chat application with authentication" should "reject messages from not authenticated clients" in {
    val network = NetworkSimulator[Message]()
    val server  = AuthenticatingServer(ChatServerBase(0), Map())
    network.registerServer(server)
    val client1 =
      AuthenticatingClient(ChatClientBase(1, server.serverId, network))
    network.registerClient(client1)

    client1.send("HelloWorld!")
    assertLastMessage(
      client1,
      server.serverId,
      "You must authenticate before sending messages."
    )
  }

  "A Chat application with authentication" should "only authenticate registered clients" in {
    val network = NetworkSimulator[Message]()
    val server =
      AuthenticatingServer(ChatServerBase(0), Map("user2" -> "securePassword"))
    network.registerServer(server)
    val client1 =
      AuthenticatingClient(ChatClientBase(1, server.serverId, network))
    network.registerClient(client1)

    client1.authenticate("user1", "password")
    assertLastMessage(client1, server.serverId, "Authentication failed.")
  }

  // ==============================================================================
  // Color tests
  // ==============================================================================
  "A Chat application with color support" should "display colored messages" in {
    val network = NetworkSimulator[Message]()
    val server  = ChatServerBase(0)
    network.registerServer(server)
    val client1 = ColoringClient(ChatClientBase(1, server.serverId, network))
    network.registerClient(client1)

    client1.send("HelloWorld!", Console.RED)
    assertLastMessage(client1, client1.clientId, "HelloWorld!", Console.RED)
  }

  "A Chat application without color support" should "not display colored messages" in {
    val network = NetworkSimulator[Message]()
    val server  = ChatServerBase(0)
    network.registerServer(server)
    val client1 = ChatClientBase(1, server.serverId, network)
    network.registerClient(client1)

    assertThrows[ConfigurationError]({
      client1.send("HelloWorld!", Console.RED)
    })
  }

  // ==============================================================================
  // Encryption tests
  // ==============================================================================
  "A Chat application with 'reverse' encryption" should "display decrypted messages" in {
    val network = NetworkSimulator[Message]()
    val server  = EncryptingServer(ChatServerBase(0), REVERSE)
    network.registerServer(server)
    val client1 =
      EncryptingClient(ChatClientBase(1, server.serverId, network), REVERSE)
    network.registerClient(client1)

    client1.send("HelloWorld!")
    assertLastMessage(client1, client1.clientId, "HelloWorld!")
  }

  "A Chat application with 'rot13' encryption" should "display decrypted messages" in {
    val network = NetworkSimulator[Message]()
    val server  = EncryptingServer(ChatServerBase(0), ROT13)
    network.registerServer(server)
    val client1 =
      EncryptingClient(ChatClientBase(1, server.serverId, network), ROT13)
    network.registerClient(client1)

    client1.send("HelloWorld!")
    assertLastMessage(client1, client1.clientId, "HelloWorld!")
  }

  // ==============================================================================
  // Logging tests
  // ==============================================================================

  "A Chat application with logging" should "log important operations" in {
    val network = NetworkSimulator[Message]()
    val server = LoggingServer(
      AuthenticatingServer(ChatServerBase(0), Map("user2" -> "securePassword"))
    )
    network.registerServer(server)
    val client1 = LoggingClient(
      AuthenticatingClient(ChatClientBase(1, server.serverId, network))
    )
    network.registerClient(client1)

    assert(
      server.logger.lastLoggedMessage() === s"New client: ${client1.clientId}"
    )

    client1.send("Hello world!")
    assert(
      server.logger
        .lastLoggedMessage() === s"Rejected message from unauthenticated client: ${client1.clientId}"
    )
    assert(
      client1.logger
        .lastLoggedMessage() === s"Received message from sender ${server.serverId}"
    )
    assertLastMessage(
      client1,
      server.serverId,
      "You must authenticate before sending messages."
    )

    client1.authenticate("user1", "password")
    assert(
      server.logger
        .lastLoggedMessage() === s"Failed to authenticate client: ${client1.clientId}"
    )
    assertLastMessage(client1, server.serverId, "Authentication failed.")

    client1.authenticate("user2", "securePassword")
    assert(
      client1.logger
        .lastLoggedMessage() === s"Sending authentication request: [${client1.clientId}] u=user2 p=securePassword"
    )
    assert(
      server.logger
        .lastLoggedMessage() === s"Successfully authenticated client: ${client1.clientId}"
    )

    client1.send("Hello world!")
    assert(
      server.logger
        .lastLoggedMessage() === s"Broadcasting message from sender ${client1.clientId}"
    )
    assert(
      client1.logger
        .lastLoggedMessage() === s"Received message from sender ${client1.clientId}"
    )
    assertLastMessage(client1, client1.clientId, "Hello world!")
  }
}
