package com.tuplejump.inventory

import scala.collection.mutable

class Inventory(id: String) {

  // items is a hashMap from (item id, terminal id) to the item
  private val items: mutable.HashMap[(String, String), Item] =
    mutable.HashMap.empty[(String, String), Item]

  def searchItem(code: String, terminal: String): Item = {
    items.getOrElse((code, terminal), null)
  }

  def addItem(item: Item, code: String, terminal: String) {
    if (items.getOrElse((code, terminal), "") == "") {
      items((code, terminal)) = item
    }
    else {
      changeQuantity(code, item.getQuantity, terminal, terminal)
    }
  }

  def getInfo: mutable.HashMap[(String, String), Int] = {
    for(item <- items) yield {
      (item._1, item._2.getQuantity)
    }
  }

  def changeQuantity(
                      code: String,
                      quantity: Int,
                      node: String,
                      terminal: String) {
    val item = searchItem(code, terminal)
    item.changeQuantity(node, quantity)
    items((code, terminal)) = item
  }

  def placeOrder(
                  code: String,
                  quantity: Int,
                  node: String,
                  terminal: String): Boolean = {
    var isOrdered = false
    val item = searchItem(code, terminal)
    if (item != null) {
      if (item.getQuantity >= quantity) {
        changeQuantity(code, -quantity, node, terminal)
        isOrdered = true
      }
      else isOrdered = false
    }
    isOrdered
  }

  def canPlaceOrder(
                     code: String,
                     quantity: Int,
                     node: String,
                     terminal: String): Boolean = {
    var canOrder = false
    val item = searchItem(code, terminal)
    if (item != null) {
      if (item.getQuantity >= quantity) {
        canOrder = true
      }
      else {
        canOrder = false
      }
    }
    canOrder
  }

  def merge(another: Inventory) {
    for (key <- another.items.keySet) {
      items(key).quantity =
        another.items(key).quantity.merge(items(key).quantity)
    }
  }

}
