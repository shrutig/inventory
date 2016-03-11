package com.tuplejump.inventory

import scala.collection.mutable
import akka.actor.{ActorRef, Props, FSM, Actor}
import scala.concurrent.duration._
import akka.util.Timeout
import org.jboss.aesh.console.Console

class NodeManager(terminal: String, inventory: Inventory)
  extends Actor with FSM[State, Data] {

  val terminalName = terminal

  import context._

  system.scheduler.schedule(1 second, 4 seconds) {
    self ! Speak("send changes")
  }

  val journal = mutable.HashMap.empty[ActorRef, Seq[(Change, Long)]]

  implicit val timeout = Timeout(5 seconds)

  startWith(ChatOffline, Uninitialized)

  when(ChatOffline) {
    case Event(CreateChat(nodes), Uninitialized) =>
      log.info("CreateChat received while in ChatOffline state.")
      goto(ChatOnline) using ChatData(nodes)
    case Event(StartChat, ChatData(nodes)) =>
      log.info("StartChat received while in ChatOffline state.")
      goto(ChatOnline) using ChatData(nodes)
    case Event(add: Add, ChatData(nodes)) =>
      if (add.terminal == terminal) addItem(add, nodes)
      stay
    case Event(purchase: Purchase, ChatData(nodes)) =>
      if (purchase.pOSTerminal == terminal) placeOrder(purchase, nodes)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    ChatData(nodes)) =>
      if (pOSTerminal == terminal)
        console.getShell.out.println(
          inventory.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event(("show", console: Console), ChatData(nodes)) =>
      show(console)
    case Event(Speak(text), ChatData(nodes)) =>
      stay
    case Event(journal: Journal, ChatData(nodes)) =>
      stay
    case Event(verify: VerifyAck, ChatData(nodes)) =>
      stay
  }

  def show(console: Console) = {
    console.getShell.out.println()
    val info = inventory.getInfo
    info.foreach {
      case ((itemCode, terminalId), qty) =>
        console.getShell.out.println(itemCode + " " + terminalId + " " + qty)
    }
    console.getShell.out.println()
    stay
  }

  def now = System.currentTimeMillis()

  def addItem(add: Add, nodes: List[ActorRef]) = {
    inventory.addItem(new Item(
      add.itemType,
      add.itemMake,
      PNCounter(PCounter(add.quantity), NCounter())),
      add.itemCode,
      add.terminal)
    if (add.terminal == terminal)
      nodes.foreach { act =>
        val currentChanges = journal.getOrElse(act, Seq[(Change, Long)]())
        journal += (act -> (currentChanges :+
          (Change(
            add.itemCode,
            add.terminal,
            Some(inventory.searchItem(add.itemCode, add.terminal)
              .quantity.increments.value),
            None,
            add.terminal), now)))
      }
  }

  def placeOrder(purchase: Purchase, nodes: List[ActorRef]) = {
    inventory.placeOrder(
      purchase.itemCode,
      purchase.quantity,
      purchase.pOSTerminal,
      purchase.terminal)
    if (purchase.pOSTerminal == terminal)
      nodes.foreach { act =>
        val currentChanges = journal.getOrElse(act, Seq[(Change, Long)]())
        journal += (act -> (currentChanges :+
          (Change(
            purchase.itemCode,
            purchase.terminal,
            None,
            Some(inventory.searchItem(purchase.itemCode, purchase.terminal)
              .quantity.decrements.state(purchase.pOSTerminal)),
            purchase.pOSTerminal), now)))
      }
  }

  when(ChatOnline) {
    case Event(("show", console: Console), ChatData(nodes)) =>
      show(console)
    case Event(journal: Journal, ChatData(nodes)) =>
      val conflicts = inventory.applyJournal(journal.list)
      if(conflicts.nonEmpty)
        sender() ! Conflict(conflicts)
      sender() ! VerifyAck(journal.list.last._1, journal.list.last._2)
      stay
    case Event(conflict: Conflict, ChatData(nodes)) =>
      //TODO resolve conflict: Tell customer and make items to 0 after checks
      stay
    case Event(verify: VerifyAck, ChatData(nodes)) =>
      val currentJournal = journal(sender())
      val index = currentJournal.indexOf((verify.change, verify.time))
      journal(sender()) = currentJournal.drop(index)
      stay
    case Event(Speak("send changes"), ChatData(nodes)) =>
      journal.foreach(act => act._1 ! Journal(journal(act._1)))
      stay
    case Event(add: Add, ChatData(nodes)) => addItem(add, nodes)
      stay
    case Event(purchase: Purchase, ChatData(nodes)) =>
      placeOrder(purchase, nodes)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    ChatData(nodes)) =>
      console.getShell.out.println(
        inventory.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event(StopChat, ChatData(nodes)) =>
      log.info("StopChat event received while in ChatOnline state.")
      goto(ChatOffline) using ChatData(nodes)
  }

  whenUnhandled {
    case Event(e, s) =>
      log.warning("received unhandled request {} ", e)
      stay
  }
}

object NodeManager {
  def props(terminal: String, inventory: Inventory): Props =
    Props(new NodeManager(terminal: String, inventory: Inventory))
}
