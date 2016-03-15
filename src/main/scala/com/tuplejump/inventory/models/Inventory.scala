package com.tuplejump.inventory.models

import scala.collection.mutable
import com.tuplejump.inventory.service.Change

trait Inventory {
  def searchItem(code: String, terminal: String): Item

  def addItem(item: Item, code: String, terminal: String)

  def getInfo: mutable.HashMap[(String, String), Int]

  def changeQuantity(
                      code: String,
                      quantity: Int,
                      node: String,
                      terminal: String)

  def placeOrder(
                  code: String,
                  quantity: Int,
                  pOSTerminal: String,
                  terminal: String): Boolean

  def canPlaceOrder(
                     code: String,
                     quantity: Int,
                     terminal: String): Boolean

  def merge(another: InMemInventory)

  def applyJournal(journal: Seq[(Change, Long)]):
  mutable.ArrayBuffer[(String, String, Int)]

}