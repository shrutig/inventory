package com.tuplejump.inventory

import scala.collection.mutable
import akka.actor.ActorRef

sealed trait Event

case class Start(nodes: List[ActorRef]) extends Event

case class GoOnline() extends Event

case class GoOffline() extends Event

case class Show() extends Event

case class Sync() extends Event

case class KillChat() extends Event

case class VerifyAck(change: Change, time: Long) extends Event

case class Journal(list: Seq[(Change, Long)]) extends Event

case class Change(code: String,
                  terminal: String,
                  PCounter: Option[Int],
                  NCounter: Option[Int],
                  POSTerminal: String) extends Event

case class Conflict(conflicts: mutable.ArrayBuffer[(String, String, Int)])
  extends Event