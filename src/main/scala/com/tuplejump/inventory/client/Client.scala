package com.tuplejump.inventory.client

import akka.actor._
import akka.remote.RemoteScope
import org.jboss.aesh.console.Console
import com.tuplejump.inventory.models.InMemInventory
import com.tuplejump.inventory.service._

class Client(num: Int) {

  val address = Address("akka.tcp", "sys", "127.0.0.1", 2553)
  val system = ActorSystem("inventory")
  val list = {
    for (i <- 1 to num) yield {
      system.actorOf(Props(new NodeManager()).
        withDeploy(Deploy(scope = RemoteScope(address))), "terminal" + i)
    }
  }.toList

  def start() =
    list.foreach(act =>
      act ! Start(list diff List(act), new InMemInventory(act.path.name)))

  /*val cloud = system.actorOf(NodeManager.props("terminal" + 1,
       new InMemInventory("terminal" + 1)).
       withDeploy(Deploy(scope = RemoteScope(address))), "terminal" + 1)
     val list = for (i <- 2 to num) yield {
       system.actorOf(NodeManager.props("terminal" + i,
         new InMemInventory("terminal" + i)).
         withDeploy(Deploy(scope = RemoteScope(address))), "terminal" + i)
     }
     cloud ! CreateChat(list.toList)
     list.foreach(act => act ! CreateChat(List(cloud)))*/

  def parse(input: String, console: Console) = {
    input.toLowerCase.trim match {
      case add: String if add.startsWith("add") =>
        val add = SimpleParser.parseAdd(input)
        findActor(add.terminal) ! add
      case returnC: String if returnC.startsWith("return") =>
        val itemReturn = SimpleParser.parseAdd(input)
        findActor(itemReturn.terminal) ! itemReturn
      case purchase: String if purchase.startsWith("purchase") =>
        val purchase = SimpleParser.parsePurchase(input)
        findActor(purchase.pOSTerminal) ! purchase
      case canPurchase: String if canPurchase.startsWith("canpurchase") =>
        val canPurchase = SimpleParser.parsePurchase(input)
        findActor(canPurchase.pOSTerminal) !(canPurchase, console)
      case offline: String if offline.startsWith("offline") =>
        val node = offline.split("offline ")(1)
        findActor(node) ! GoOffline
      case online: String if online.startsWith("online") =>
        val node = online.split("online ")(1)
        findActor(node) ! GoOnline
      case show: String if show.startsWith("show") =>
        val node = show.split("show ")(1)
        findActor(node) !(Show, console)
      case _ =>
        throw new IllegalArgumentException("Cannot parse the given statement")
    }
  }

  def help() = println(
    """Add items :
          add code=<string> terminal=terminal<int> type=<string>""".stripMargin +
      """make=<string> quantity=<int>
        |Return items :
          return code=<string> terminal=terminal<int> """.stripMargin +
      """type=<string> make=<string> quantity=<int>
        |Purchase items :
          purchase code=<string> terminal=terminal<int> quantity=<int> from <string>
        |Can Purchase items :
          canPurchase code=<string> terminal=terminal<int> quantity=<int>"""
      .stripMargin)

  def findActor(terminal: String): ActorRef = {
    list(terminal.split("terminal")(1).toInt - 1)
  }
}