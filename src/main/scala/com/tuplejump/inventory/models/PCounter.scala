package com.tuplejump.inventory.models

//Changes made from NCounter in akka.crdt
case class PCounter( state: Int = 0) {

  val dataType: String = PCounter.dataType

  def value: Int = state

  def +(delta: Int = 1): PCounter = {
    require(delta >= 0, "Can't decrement a PCounter")
    copy(state = state + delta)
  }

  def merge(that: PCounter): PCounter = {
    copy(state = Math.max(this.state, that.state))
  }
}

object PCounter {
  val dataType: String = "p-counter"
}
