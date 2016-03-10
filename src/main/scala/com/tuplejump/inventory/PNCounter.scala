package com.tuplejump.inventory

//Changes made from NCounter in akka.crdt
case class PNCounter(
                      increments: PCounter,
                      decrements: NCounter) {
  val dataType: String = PNCounter.dataType

  def value = increments.value - decrements.value

  /**
    * Increment the PNCounter with the delta specified. If the value is
    * negative then it will decrement instead of increment.
    */
  def +(node: String, delta: Int = 1): PNCounter = {
    if (delta < 0) this -(node, delta)
    else new PNCounter(increments + delta, decrements)
  }

  /**
    * Decrements the PNCounter with the delta specified. Agnostic to
    * sign (does Math.abs(delta)).
    */
  def -(node: String, delta: Int = 1): PNCounter =
    new PNCounter(increments, decrements +(node, Math.abs(delta)))

  def merge(that: PNCounter): PNCounter =
    new PNCounter(
      that.increments.merge(this.increments),
      that.decrements.merge(this.decrements))

}

object PNCounter {
  val dataType: String = "pn-counter"
}

