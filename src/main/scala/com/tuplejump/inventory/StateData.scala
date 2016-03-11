package com.tuplejump.inventory

import scala.collection.mutable
import akka.actor.ActorRef

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

case class VerifyAck(change: Change, time: Long)

case class Journal(list: Seq[(Change, Long)])

case class Change(code: String,
                  terminal: String,
                  PCounter: Option[Int],
                  NCounter: Option[Int],
                  POSTerminal: String)

case class Speak(text: String)

case class Conflict(conflicts: mutable.ArrayBuffer[(String, Int)])