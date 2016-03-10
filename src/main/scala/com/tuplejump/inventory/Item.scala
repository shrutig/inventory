package com.tuplejump.inventory

class Item(
           val `type`: String,
           val make: String,
           var quantity: PNCounter = PNCounter(PCounter(), NCounter())) {

  def getQuantity: Int = {
    quantity.value
  }

  def changeQuantity(node: String, change: Int) {
    this.quantity = quantity +(node, change)
  }

}

