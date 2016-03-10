package com.tuplejump.inventory

import akka.actor._
import akka.remote.RemoteScope
import scala.io.StdIn
import org.jboss.aesh.console._
import org.jboss.aesh.console.settings.SettingsBuilder

object InventoryApplication {

  def main(args: Array[String]) {
    val address = Address("akka.tcp", "sys", "127.0.0.1", 2553)
    val system = ActorSystem("inventory")
    print("Enter number of POS terminals: ")
    val num = StdIn.readInt()
    val list = for (i <- 1 to num) yield {
      system.actorOf(NodeManager.props("terminal" + i,
        new Inventory("terminal" + i)).
        withDeploy(Deploy(scope = RemoteScope(address))), "terminal" + i)
    }
    list.foreach(act => act ! CreateChat((list diff List(act)).toList))
    val flag = true
    val builder: SettingsBuilder = new SettingsBuilder().ansi(true)
    builder.logging(true).logfile(System.getProperty("user.dir") +
      System.getProperty("file.separator") + "akka.log")
    builder.persistHistory(true)
    builder.parseOperators(false)
    val console: Console = new Console(builder.create)
    val consoleCallback: ConsoleCallback = new ConsoleCallBack(console, list)
    console.setConsoleCallback(consoleCallback)
    val prompt: Prompt = new Prompt("Enter command>")
    console.start()
    console.setPrompt(prompt)
  }
}
