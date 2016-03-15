package com.tuplejump.inventory.service

import scala.collection.mutable
import akka.actor.ActorRef
import com.tuplejump.inventory.models.Inventory

sealed trait Event

trait Command extends Event

trait StateChangeCommand extends Command

case class Start(nodes: List[ActorRef], inv: Inventory) extends StateChangeCommand

case object GoOnline extends StateChangeCommand

case object GoOffline extends StateChangeCommand

case object Show extends Command

case class Add(itemCode: String,
               terminal: String,
               itemType: String,
               itemMake: String,
               quantity: Int) extends Command

case class Purchase(itemCode: String,
                    terminal: String,
                    quantity: Int,
                    pOSTerminal: String) extends Command

trait InterNodeEvent extends Event

case object Sync extends InterNodeEvent

case class VerifyAck(change: Change, time: Long) extends InterNodeEvent

case class Journal(list: Seq[(Change, Long)]) extends InterNodeEvent

case class Conflict(conflicts: mutable.ArrayBuffer[(String, String, Int)])
  extends InterNodeEvent
