package com.tuplejump.inventory.cli

import org.jboss.aesh.console.{AeshConsoleCallback, Console, ConsoleOperation}
import com.tuplejump.inventory.client.Client

// Some code used from trulite/index of github.com/tuplejump/filodb
class ConsoleCallBack(console: Console, cli: Client)
  extends AeshConsoleCallback {

  def execute(output: ConsoleOperation) = {
    output.getBuffer.toLowerCase match {
      case "quit" | "exit" | "reset" =>
        console.getShell.out.println()
        console.stop()
        System.exit(0)
      case "clear" => console.clear()
      case "help" => cli.help()
      case command =>
        cli.parse(command, console)
    }
    1
  }

}
