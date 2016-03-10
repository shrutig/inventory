package com.tuplejump.inventory

import akka.actor.{Props, ActorRef, FSM, Actor}
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout
import org.jboss.aesh.console.Console
class NodeManager(terminal: String, inventory: Inventory)
  extends Actor with FSM[State, Data] {

  /*import context._

  system.scheduler.schedule(1 second, 3 seconds) {
    self ! "merge"
  }
  */
  implicit val timeout = Timeout(5 seconds)
  import scala.concurrent.ExecutionContext.Implicits.global
  startWith(ChatOffline, Uninitialized)

  when(ChatOffline) {
    case Event(CreateChat(nodes), Uninitialized) =>
      log.info("CreateChat received while in ChatOffline state.")
      goto(ChatOnline) using ChatData(nodes)
    case Event(StartChat, ChatData(nodes)) =>
      log.info("StartChat received while in ChatOffline state.")
      nodes.foreach { act => val future = ask(act, "inventory").mapTo[Inventory]
        future.onSuccess {
          case result: Inventory ⇒ inventory.merge(result)
        }
        act ! "merge"
      }
      goto(ChatOnline) using ChatData(nodes)
    case Event((Add(code, iTerminal, iType, make, quantity)),
    ChatData(nodes)) =>
      if (iTerminal == terminal)
        inventory.addItem(
          new Item(iType, make, PNCounter(PCounter(quantity), NCounter())),
          code,
          terminal)
      stay
    case Event((Purchase(code, iTerminal, quantity, returnBool)),
    ChatData(nodes)) =>
      if (iTerminal == terminal) {
        if (returnBool)
          inventory.canPlaceOrder(code, quantity, terminal, terminal)
        else
          inventory.placeOrder(code, quantity, terminal, terminal)
      }
      stay
    case Event(("show", console: Console), ChatData(nodes)) =>
      val info = inventory.getInfo
      info.foreach{
        case ((itemCode, terminalId),qty)=> log.info(itemCode + " " +
          terminalId + " " + qty)
      }
      stay
    case Event("merge", ChatData(nodes)) =>
      stay
  }

  when(ChatOnline) {
    case Event(("show", console: Console), ChatData(nodes)) =>
      val info = inventory.getInfo
      info.foreach{
        case ((itemCode, terminalId),qty)=>
          console.getShell.out.println(itemCode + " " + terminalId + " " + qty)
      }
      stay
    case Event("inventory", ChatData(nodes)) => sender() ! inventory
      stay
    case Event("merge", ChatData(nodes)) =>
      nodes.foreach { act => val future = ask(act, "inventory").mapTo[Inventory]
        future.onSuccess {
          case result: Inventory ⇒ inventory.merge(result)
        }
      }
      stay
    case Event((Add(code, iTerminal, iType, make, quantity)),
    ChatData(nodes)) =>
      inventory.addItem(new Item(
        iType,
        make,
        PNCounter(PCounter(quantity), NCounter())),
        code,
        iTerminal)
      if (iTerminal == terminal)
        nodes.foreach(act =>
          act ! Add(code, iTerminal, iType, make, quantity))
      stay
    case Event(Purchase(code, iTerminal, quantity, returnBool),
    ChatData(nodes)) =>
      if (returnBool)
        inventory.canPlaceOrder(code, quantity, terminal, terminal)
      else
        inventory.placeOrder(code, quantity, terminal, terminal)
      if (iTerminal == terminal)
        nodes.foreach(act =>
          act ! Purchase(code, iTerminal, quantity, returnBool))
      stay
    case Event(StopChat, ChatData(nodes)) =>
      log.info("StopChat event received while in ChatOnline state.")
      goto(ChatOffline) using ChatData(nodes)
  }

  whenUnhandled {
    case Event(KillChat, ChatData(_)) =>
      println("Shutting down...\n\n")
      context.system.terminate()
      stay
    case Event(e, s) =>
      log.warning(
        "received unhandled request {} in state {}/{}",
        e,
        stateName,
        s)
      stay
  }

  def process(text: String) = {
    log.info(text)
  }
}

object NodeManager {
  def props(terminal: String, inventory: Inventory): Props =
    Props(new NodeManager(terminal: String, inventory: Inventory))
}

sealed trait State

case object ChatOffline extends State

case object ChatOnline extends State

sealed trait Data

case object Uninitialized extends Data

case class ChatData(nodes: List[ActorRef]) extends Data

case class CreateChat(nodes: List[ActorRef])

case class StartChat()

case class StopChat()

case class KillChat()

