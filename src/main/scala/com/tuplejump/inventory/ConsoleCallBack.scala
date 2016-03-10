package com.tuplejump.inventory

import akka.actor.ActorRef
import org.jboss.aesh.console.{ConsoleOperation, AeshConsoleCallback, Console}
// Some code used from trulite/index of github.com/tuplejump/filodb
class ConsoleCallBack(console: Console, list: IndexedSeq[ActorRef])
  extends AeshConsoleCallback {

  def execute(output: ConsoleOperation) = {
    output.getBuffer.toLowerCase match {
      case "quit" | "exit" | "reset" =>
        console.getShell.out.println()
        console.stop()
        System.exit(0)
      case "clear" => console.clear()
      case "help" => println(
        """Add items :
          add code=<string> terminal=terminal<int> type=<string>""".stripMargin +
          """make=<string> quantity=<int>
            |Return items :
          return code=<string> terminal=terminal<int> """.stripMargin +
          """type=<string> make=<string> quantity=<int>
            |Purchase items :
          purchase code=<string> terminal=terminal<int> quantity=<int>
            |Can Purchase items :
          canPurchase code=<string> terminal=terminal<int> quantity=<int>"""
          .stripMargin)
      case command =>
        interpret(command)
    }
    1
  }

  def interpret(input: String) = {
    input.toLowerCase.trim match {
      case add: String if add.startsWith("add") =>
        val add = SimpleParser.parseAdd(input)
        findActor(add.terminal) ! add
      case returnC: String if returnC.startsWith("return") =>
        val itemReturn = SimpleParser.parseAdd(input)
        findActor(itemReturn.terminal) ! itemReturn
      case purchase: String if purchase.startsWith("purchase") =>
        val purchase = SimpleParser.parsePurchase(input)
        findActor(purchase.terminal) ! purchase
      case canPurchase: String if canPurchase.startsWith("canpurchase") =>
        val canPurchase = SimpleParser.parsePurchase(input)
        findActor(canPurchase.terminal) ! canPurchase
      case offline: String if offline.startsWith("offline") =>
        val node = offline.split("offline ")(1)
        findActor(node) ! StopChat
      case online:String if online.startsWith("online") =>
        val node = online.split("offline ")(1)
        findActor(node) ! StartChat
      case show: String if show.startsWith("show") =>
        val node = show.split("show ")(1)
        findActor(node) ! ("show", console)

      case _ =>
        throw new IllegalArgumentException("Cannot parse the given statement")
    }
  }

  def findActor(terminal: String): ActorRef = {
    list(terminal.split("terminal")(1).toInt - 1)
  }
}
