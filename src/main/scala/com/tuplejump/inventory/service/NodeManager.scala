package com.tuplejump.inventory.service

import scala.collection.mutable
import scala.concurrent.duration._
import akka.actor.{Actor, ActorRef, FSM, Props}
import akka.util.Timeout
import org.jboss.aesh.console.Console
import com.tuplejump.inventory.models._

class NodeManager extends Actor with FSM[State, Data] {

  import context._

  system.scheduler.schedule(1 second, 4 seconds) {
    self ! Sync
  }

  val journal = mutable.HashMap.empty[ActorRef, Seq[(Change, Long)]]

  implicit val timeout = Timeout(5 seconds)

  startWith(Offline, Uninitialized)

  when(Offline) {
    case Event(Start(nodes, inventory), Uninitialized) =>
      log.info("Start received while in Offline state.")
      goto(Online) using DataInv(nodes, inventory)
    case Event(GoOnline, DataInv(nodes, inv)) =>
      log.info("GoOnline received while in Offline state.")
      goto(Online) using DataInv(nodes, inv)
    case Event(add: Add, DataInv(nodes, inv)) => addItem(add, nodes, inv)
      stay
    case Event(purchase: Purchase, DataInv(nodes, inv)) =>
      placeOrder(purchase, nodes, inv)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    DataInv(nodes, inv)) =>
      console.getShell.out.println(
        inv.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event((Show, console: Console), DataInv(nodes, inv)) =>
      show(console, inv)
    case Event(event: InterNodeCommand, DataInv(nodes, inv)) =>
      stay
    case Event(GoOffline, DataInv(nodes, inv)) =>
      stay
  }

  def show(console: Console, inventory: Inventory) = {
    val info = inventory.getInfo
    info.foreach {
      case ((itemCode, terminalId), qty) =>
        console.getShell.out.println(itemCode + " " + terminalId + " " + qty)
    }
    stay
  }

  def now = System.currentTimeMillis()

  def addItem(add: Add, nodes: List[ActorRef], inventory: Inventory) = {
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

  def placeOrder(purchase: Purchase, nodes: List[ActorRef], inv: Inventory) = {
    inv.placeOrder(
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
          Some(inv.searchItem(purchase.itemCode, purchase.terminal)
            .quantity.decrements.state(purchase.pOSTerminal)),
          purchase.pOSTerminal), now)))
    }
  }

  when(Online) {
    case Event((Show, console: Console), DataInv(nodes, inv)) =>
      show(console, inv)
    case Event(journal: Journal, DataInv(nodes, inv)) =>
      val conflicts = inv.applyJournal(journal.list)
      if (conflicts.nonEmpty)
        sender() ! Conflict(conflicts)
      sender() ! VerifyAck(journal.list.last._1, journal.list.last._2)
      stay
    case Event(Conflict(conflicts), DataInv(nodes, inv)) =>
      log.info("conflicts found")
      //TODO resolve conflict: Tell customer and make items to 0 after checks
      stay
    case Event(verify: VerifyAck, DataInv(nodes, inv)) =>
      val currentJournal = journal(sender())
      val index = currentJournal.indexOf((verify.change, verify.time))
      journal(sender()) = currentJournal.drop(index)
      stay
    case Event(Sync, DataInv(nodes, inv)) =>
      journal.foreach(act => act._1 ! Journal(journal(act._1)))
      stay
    case Event(add: Add, DataInv(nodes, inv)) => addItem(add, nodes, inv)
      stay
    case Event(purchase: Purchase, DataInv(nodes, inv)) =>
      placeOrder(purchase, nodes, inv)
      stay
    case Event(
    (Purchase(code, iTerminal, quantity, pOSTerminal), console: Console),
    DataInv(nodes, inv)) =>
      console.getShell.out.println(
        inv.canPlaceOrder(code, quantity, iTerminal))
      stay
    case Event(GoOffline, DataInv(nodes, inv)) =>
      log.info("GoOffline event received while in Online state.")
      goto(Offline) using DataInv(nodes, inv)
    case Event(GoOnline, DataInv(nodes, inv)) =>
      stay
    case Event(start: Start, DataInv(nodes, inv)) =>
      stay
  }

  whenUnhandled {
    case Event(e, s) =>
      log.warning("received unhandled request {} ", e)
      stay
  }
}

sealed trait State

case object Offline extends State

case object Online extends State

sealed trait Data

case object Uninitialized extends Data

case class DataInv(nodes: List[ActorRef], inventory: Inventory) extends Data

case class Change(code: String,
                  terminal: String,
                  PCounter: Option[Int],
                  NCounter: Option[Int],
                  POSTerminal: String)

