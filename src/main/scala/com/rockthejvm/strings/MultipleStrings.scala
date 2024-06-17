package com.rockthejvm.strings

import scala.annotation.tailrec

object MultipleStrings extends App {
  // multiple two numbers represented as strings, of arbitrary length
  def multiplyStrings(a: String, b: String): String = {
    // @tailrec
    // def multiply(
    //     digits1: List[Int],
    //     digits2: List[Int],
    //     digits2OG: List[Int],
    //     multiplier1: BigInt,
    //     multiplier2: BigInt,
    //     acc: BigInt
    // ): String = {
    //   if (digits1.isEmpty) acc.toString
    //   else if (digits2.isEmpty) multiply(digits1.tail, digits2OG, digits2OG, multiplier1 * 10, 1, acc)
    //   else {
    //     val product = (digits1.head * multiplier1) * (digits2.head * multiplier2)
    //     multiply(digits1, digits2.tail, digits2OG, multiplier1, multiplier2 * 10, acc + product)
    //   }
    // }

    // List(3, 2, 1), 6 => List(8, 3, 7)
    def multiplyByDigit(number: List[Int], factor: Int): List[Int] = {
      @tailrec
      def multiplyByDigitTailrec(remainingDigits: List[Int], carry: Int, acc: List[Int]): List[Int] = {
        if (remainingDigits.isEmpty) {
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        } else {
          val newDigit   = remainingDigits.head
          val newProduct = newDigit * factor + carry

          val newCarry = newProduct / 10 // 0 or 1
          val newAcc   = (newProduct % 10) :: acc
          multiplyByDigitTailrec(remainingDigits.tail, newCarry, newAcc)
        }
      }
      multiplyByDigitTailrec(number, 0, List())
    }

    def addTwoNumbers(a: List[Int], b: List[Int]): List[Int] = {
      def addTwoTailrec(
          remainingA: List[Int],
          remainingB: List[Int],
          carry: Int = 0,
          acc: List[Int] = List()
      ): List[Int] = {
        if (remainingA.isEmpty && remainingB.isEmpty) {
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        } else if (remainingA.isEmpty) acc.reverse ++ addTwoTailrec(List(carry), remainingB)
        else if (remainingB.isEmpty) acc.reverse ++ addTwoTailrec(List(carry), remainingA)
        else {
          val newSum   = remainingA.head + remainingB.head + carry
          val newDigit = newSum % 10
          val newCarry = newSum / 10

          addTwoTailrec(remainingA.tail, remainingB.tail, newCarry, newDigit :: acc)
        }
      }

      if (a.isEmpty) b
      else if (b.isEmpty) a
      else addTwoTailrec(a, b)
    }

    // 123 * 456 -> List(3, 2, 1) * List(6, 5, 4)
    def multiplyDigits(a: List[Int], b: List[Int]): List[Int] = {
      b.zipWithIndex
        .map { case (digit, index) =>
          List.fill(index)(0) ++ multiplyByDigit(a, digit)
        }
        .reduce(addTwoNumbers)
    }

    val digits1Reversed = a.map(_ - '0').toList.reverse
    val digits2Reversed = b.map(_ - '0').toList.reverse
    val digitsResult    = multiplyDigits(digits1Reversed, digits2Reversed)

    val result = digitsResult.reverse.mkString("")

    if (result.isEmpty || result.charAt(0) == '0') "0"
    else result
  }

  println(multiplyStrings("1123124124312423", "1125362340"))
}
