package com.tuplejump.inventory.cli

import scala.io.StdIn
import org.jboss.aesh.console._
import org.jboss.aesh.console.settings.SettingsBuilder
import com.tuplejump.inventory.client.Client

object InventoryApplication {

  def main(args: Array[String]) {
    print("Enter number of POS terminals: ")
    val num = StdIn.readInt()
    val cli = new Client(num)
    cli.start()
    val builder: SettingsBuilder = new SettingsBuilder().ansi(true)
    builder.logging(true).logfile(System.getProperty("user.dir") +
      System.getProperty("file.separator") + "akka.log")
    builder.persistHistory(true)
    builder.parseOperators(false)
    val console: Console = new Console(builder.create)
    val consoleCallback: ConsoleCallback = new ConsoleCallBack(console, cli)
    console.setConsoleCallback(consoleCallback)
    val prompt: Prompt = new Prompt("Enter command>")
    console.start()
    console.setPrompt(prompt)
  }
}
