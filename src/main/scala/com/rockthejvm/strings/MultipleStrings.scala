package com.rockthejvm.strings

import scala.annotation.tailrec

object MultipleStrings extends App {
  // multiple two numbers represented as strings, of arbitrary length
  def multiplyStrings(a: String, b: String): String = {
    @tailrec
    def multiply(
        digits1: List[Int],
        digits2: List[Int],
        digits2OG: List[Int],
        multiplier1: BigInt,
        multiplier2: BigInt,
        acc: BigInt
    ): String = {
      if (digits1.isEmpty) acc.toString
      else if (digits2.isEmpty) multiply(digits1.tail, digits2OG, digits2OG, multiplier1 * 10, 1, acc)
      else {
        val product = (digits1.head * multiplier1) * (digits2.head * multiplier2)
        multiply(digits1, digits2.tail, digits2OG, multiplier1, multiplier2 * 10, acc + product)
      }
    }

    val digits1Reversed = a.map(_ - '0').toList.reverse
    val digits2Reversed = b.map(_ - '0').toList.reverse
    multiply(digits1Reversed, digits2Reversed, digits2Reversed, 1, 1, 0)
  }

  println(multiplyStrings("1123124124312423", "1125362340"))
}
