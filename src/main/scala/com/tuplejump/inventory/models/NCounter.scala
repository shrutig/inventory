package com.tuplejump.inventory.models

import java.util.UUID

//Changes made from NCounter in akka.crdt
case class NCounter(
                     id: String = UUID.randomUUID.toString,
                     state: Map[String, Int] = Map.empty[String, Int]){

  val dataType: String = NCounter.dataType

  def value: Int = state.values.sum

  def +(node: String, delta: Int = 1): NCounter = {
    require(delta >= 0, "Can't decrement a NCounter")
    if (state.contains(node)) copy(state = state + (node -> (state(node) + delta)))
    else copy(state = state + (node -> delta))
  }

  def merge(that: NCounter): NCounter = {
    (this.state.keySet ++ that.state.keySet).foldLeft(NCounter(id = id)) {
      (counter, key) ⇒ counter + (key, Math.max(
        this.state.getOrElse(key,0),
        that.state.getOrElse(key,0)))
    }
  }
}

object NCounter {
  val dataType: String = "n-counter"
}
