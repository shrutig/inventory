package com.tuplejump.inventory

import scala.collection.mutable
import akka.actor.ActorRef

sealed trait Event

case class Start(nodes: List[ActorRef]) extends Event

case object GoOnline extends Event

case object GoOffline extends Event

case object Show extends Event

case object Sync extends Event

case object KillChat extends Event

case class VerifyAck(change: Change, time: Long) extends Event

case class Journal(list: Seq[(Change, Long)]) extends Event

case class Change(code: String,
                  terminal: String,
                  PCounter: Option[Int],
                  NCounter: Option[Int],
                  POSTerminal: String)

case class Conflict(conflicts: mutable.ArrayBuffer[(String, String, Int)])
  extends Event