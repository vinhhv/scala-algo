package com.rockthejvm.numbers

object ReverseInteger extends App {

  /**
   * return a number with the digits reversed
   * if the result overflows Int, return 0
   */
  def reverseInteger(number: Int): Int = {
    def convertIntegerFormat(numberStr: String, minmaxStr: String): String = {
      val numZeroes       = minmaxStr.length - numberStr.length
      val formattedNumber = (1 to numZeroes).map(_ => "0").mkString("") + numberStr
      formattedNumber
    }

    if (number >= 0) {
      val reversedStr = number.toString.reverse
      if (convertIntegerFormat(reversedStr, Int.MaxValue.toString) > Int.MaxValue.toString) 0
      else reversedStr.toInt
    } else {
      val reversedStr = number.toString.tail.reverse
      if (convertIntegerFormat(reversedStr, Int.MinValue.toString.tail) > Int.MinValue.toString.tail) 0
      else s"-${reversedStr}".toInt
    }
  }

  println(reverseInteger(67899))
  println(reverseInteger(99876542))
  println(reverseInteger(-999999991))
  println(reverseInteger(Int.MaxValue))
  println(reverseInteger(Int.MinValue))
  println(reverseInteger(-1))
  println(Int.MaxValue)
}
