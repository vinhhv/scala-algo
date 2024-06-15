package com.rockthejvm.numbers

import scala.annotation.tailrec

object ParseInteger extends App {

  /**
   * Return a number from the string argument:
   * - there may be leading spaces, ignore those
   * - read the sign character if present
   * - read all the digits until the end of string or until a non-digit character
   * - return the number formed from those digits
   * - if the number exceeds the int range, return either Int.MinValue or Int.MaxValue
   */
  def parseInteger(str: String): Int = {
    @tailrec
    def parseTailrec(remaining: String, isPositive: Boolean, acc: Int): Int = {
      if (remaining.isEmpty && isPositive) acc
      else if (remaining.isEmpty && !isPositive) -acc
      else if (!remaining.head.isDigit && remaining.head != '-') parseTailrec(remaining.tail, isPositive, acc)
      else if (remaining.head == '-') parseTailrec(remaining.tail, false, acc)
      else {
        val updatedInt = acc * 10 + (remaining.head - '0').toInt
        if (updatedInt < acc && isPositive) Int.MaxValue
        else if (updatedInt < acc) Int.MinValue
        else parseTailrec(remaining.tail, isPositive, updatedInt)
      }
    }

    parseTailrec(str, true, 0)
  }

  println(parseInteger("-1000"))
  println(parseInteger("1000"))
  println(parseInteger("0"))
  println(parseInteger("0"))
  println(parseInteger("-999999999999"))
  println(parseInteger("999999999999"))
  println(parseInteger("12345"))
  println(parseInteger("     +12345"))
  println(parseInteger("asdfb"))
}
