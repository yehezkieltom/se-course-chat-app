package de.uni_saarland.cs.se
package util

import scala.collection.mutable

/** Server interface used by the network simulator. The different ChatServers
  * must implement this.
  *
  * @tparam MessageTy
  *   the types of messages the server can handle.
  */
trait Server[MessageTy] {
  def handleMessage(message: MessageTy): Unit
  def acceptClientConnection(
      clientConnection: ClientConnection[MessageTy]
  ): Unit
}

/** Client interface used by the network simulator. The different ChatClients
  * must implement this.
  *
  * @tparam MessageTy
  *   the types of messages the client can handle.
  */
trait Client[MessageTy] {
  def handleMessage(message: MessageTy): Unit
}

/** Connection class used by a server to communicate with a client.
  */
class ClientConnection[MessageTy](
    val clientId: Int,
    private val network: NetworkSimulator[MessageTy]
) {
  def sendMessage(message: MessageTy): Unit = {
    network
      .getClient(clientId)
      .map(client => client.handleMessage(message))
      .orElse(throw IllegalStateException(s"Unknown client: $clientId"))
  }
}

/** Connection class used by a client to communicate with a server.
  */
class ServerConnection[MessageTy](
    val serverId: Int,
    private val network: NetworkSimulator[MessageTy]
) {
  def sendMessage(message: MessageTy): Unit = {
    network
      .getServer(serverId)
      .map(server => server.handleMessage(message))
      .orElse(throw IllegalStateException(s"Unknown server: $serverId"))
  }
}

/** This class simulates a network that can be used by clients and servers to
  * communicate with each other.
  *
  * This is used instead of real network communication to simplify
  * implementation and testing of this assignment.
  */
class NetworkSimulator[MessageTy] {
  private var idCounter = 0;
  private val servers: mutable.Map[Int, Server[MessageTy]] =
    new mutable.HashMap()
  private val clients: mutable.Map[Int, Client[MessageTy]] =
    new mutable.HashMap()

  def getServer(serverId: Int): Option[Server[MessageTy]] =
    servers.get(serverId)
  def getClient(clientId: Int): Option[Client[MessageTy]] =
    clients.get(clientId)

  /** Register a server with the network. The returned Id acts like a network
    * address.
    */
  def registerServer(server: Server[MessageTy]): Int = {
    val serverId = idCounter
    idCounter += 1
    servers.put(serverId, server)
    serverId
  }

  /** Register a client with the network. The returned Id acts like a network
    * address.
    */
  def registerClient(client: Client[MessageTy]): Int = {
    val clientId = idCounter
    idCounter += 1
    clients.put(clientId, client)
    clientId
  }

  /** Connect the given client to the given server.
    *
    * This method asks the server to accept the connection to the client and, if
    * successful, returns a server connection that the client can use to
    * communicate with the server.
    */
  def connectToServer(
      clientId: Int,
      serverId: Int
  ): Option[ServerConnection[MessageTy]] = {
    for
      server <- servers.get(serverId)
    yield {
      server.acceptClientConnection(new ClientConnection(clientId, this))
      new ServerConnection[MessageTy](serverId, this)
    }
  }
}
