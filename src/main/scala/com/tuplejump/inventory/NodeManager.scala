package com.tuplejump.inventory

import scala.collection.mutable
import akka.actor.{ActorRef, Props, FSM, Actor}
import scala.concurrent.duration._
import akka.util.Timeout
import org.jboss.aesh.console.Console

class NodeManager(terminal: String, inventory: InMemInventory)
  extends Actor with FSM[State, Data] {

  val terminalName = terminal

  import context._

  system.scheduler.schedule(1 second, 4 seconds) {
    self ! Sync
  }

  val journal = mutable.HashMap.empty[ActorRef, Seq[(Change, Long)]]

  implicit val timeout = Timeout(5 seconds)

  startWith(Offline, Uninitialized)

  when(Offline) {
    case Event(Start(nodes), Uninitialized) =>
      log.info("Start received while in Offline state.")
      goto(Online) using Nodes(nodes)
    case Event(GoOnline, Nodes(nodes)) =>
      log.info("GoOnline received while in Offline state.")
      goto(Online) using Nodes(nodes)
    case Event(add: Add, Nodes(nodes)) => addItem(add, nodes)
      stay
    case Event(purchase: Purchase, Nodes(nodes)) =>
      placeOrder(purchase, nodes)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    Nodes(nodes)) =>
        console.getShell.out.println(
          inventory.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event((Show, console: Console), Nodes(nodes)) =>
      show(console)
    case Event(Journal, Nodes(nodes)) =>
      stay
    case Event(VerifyAck, Nodes(nodes)) =>
      stay
    case Event(Sync, Nodes(nodes)) =>
      stay
  }

  def show(console: Console) = {
    val info = inventory.getInfo
    info.foreach {
      case ((itemCode, terminalId), qty) =>
        console.getShell.out.println(itemCode + " " + terminalId + " " + qty)
    }
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

  when(Online) {
    case Event((Show, console: Console), Nodes(nodes)) =>
      show(console)
    case Event(journal: Journal, Nodes(nodes)) =>
      val conflicts = inventory.applyJournal(journal.list)
      if(conflicts.nonEmpty)
        sender() ! Conflict(conflicts)
      sender() ! VerifyAck(journal.list.last._1, journal.list.last._2)
      stay
    case Event(Conflict(conflicts), Nodes(nodes)) =>
      log.info("conflicts found")
      //TODO resolve conflict: Tell customer and make items to 0 after checks
      stay
    case Event(verify: VerifyAck, Nodes(nodes)) =>
      val currentJournal = journal(sender())
      val index = currentJournal.indexOf((verify.change, verify.time))
      journal(sender()) = currentJournal.drop(index)
      stay
    case Event(Sync, Nodes(nodes)) =>
      journal.foreach(act => act._1 ! Journal(journal(act._1)))
      stay
    case Event(add: Add, Nodes(nodes)) => addItem(add, nodes)
      stay
    case Event(purchase: Purchase, Nodes(nodes)) =>
      placeOrder(purchase, nodes)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    Nodes(nodes)) =>
      console.getShell.out.println(
        inventory.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event(GoOffline, Nodes(nodes)) =>
      log.info("GoOffline event received while in Online state.")
      goto(Offline) using Nodes(nodes)
  }

  whenUnhandled {
    case Event(e, s) =>
      log.warning("received unhandled request {} ", e)
      stay
  }
}

object NodeManager {
  def props(terminal: String, inventory: InMemInventory): Props =
    Props(new NodeManager(terminal: String, inventory: InMemInventory))
}

sealed trait State

case object Offline extends State

case object Online extends State

sealed trait Data

case object Uninitialized extends Data

case class Nodes(nodes: List[ActorRef]) extends Data

