package com.tuplejump.inventory.client

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import com.tuplejump.inventory.service._

object SimpleParser extends RegexParsers with JavaTokenParsers {

  private def handleError(e: Error, input: String) = {
    val msg = "Cannot parse [" + input + "] because " + e.msg
    throw new IllegalArgumentException(msg)
  }

  private def handleFailure(f: Failure, input: String) = {
    val msg = "Cannot parse [" + input + "] because " + f.msg
    throw new IllegalArgumentException(msg)
  }

  def parseAdd(input: String): Add =
    parseAll(add, input.toLowerCase) match {
      case s: Success[_] => s.get.asInstanceOf[Add]
      case e: Error => handleError(e, input)
      case f: Failure => handleFailure(f, input)
    }

  def parsePurchase(input: String): Purchase =
    parseAll(purchase, input.toLowerCase) match {
      case s: Success[_] => s.get.asInstanceOf[Purchase]
      case e: Error => handleError(e, input)
      case f: Failure => handleFailure(f, input)
    }

  lazy val add: Parser[Add] =
    (ADD | RETURN) ~ (CODE ~> "=" ~> ident) ~ (TERMINAL ~> "=" ~> ident) ~
      (TYPE ~> "=" ~> ident).? ~
      (MAKE ~> "=" ~> ident).? ~ (QUANTITY ~> "=" ~> wholeNumber) ^^ {
      case a ~ code ~ terminal ~ itemType ~ make ~ quantity =>
        Add(
          code,
          terminal,
          itemType.getOrElse(""),
          make.getOrElse(""),
          quantity.toInt)
    }

  lazy val purchase: Parser[Purchase] =
    (PURCHASE | CAN_PURCHASE) ~ (CODE ~> "=" ~> ident) ~
      (TERMINAL ~> "=" ~> ident) ~ (QUANTITY ~> "=" ~> wholeNumber) ~
      (FROM ~> ident) ^^ {
      case p ~ code ~ terminal ~ quantity ~ from =>
          Purchase(code, terminal, quantity.toInt, from)
    }

  protected val ADD = Keyword("add")
  protected val CODE = Keyword("code")
  protected val TERMINAL = Keyword("terminal")
  protected val TYPE = Keyword("type")
  protected val MAKE = Keyword("make")
  protected val QUANTITY = Keyword("quantity")
  protected val RETURN = Keyword("return")
  protected val PURCHASE = Keyword("purchase")
  protected val CAN_PURCHASE = Keyword("canpurchase")
  protected val FROM = Keyword("from")

  case class Keyword(key: String)

  // Convert the keyword into an case insensitive Parser
  implicit def keyword2Parser(kw: Keyword): Parser[String] = {
    ("""(?i)\Q""" + kw.key + """\E""").r
  }
}