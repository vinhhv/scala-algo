package com.rockthejvm.numbers

object LargestNumber extends App {

  /**
    * Given a list of non-negative integers, arrange them such that they form the largest number.
    * The result might be huge so return a string
    *
    * List(10, 2) => "210"
    * List(3, 30, 5, 9, 34) => "9534330"
    */
  def largestNumber(numbers: List[Int]): String = {
    given newOrdering: Ordering[Int] = Ordering.fromLessThan({ (a, b) =>
      // concatenate a with b => ab
      // concatenate b with a => ba
      val ab = a.toString + b.toString
      val ba = b.toString + a.toString

      ab > ba
    })

    val largest = numbers.sorted(newOrdering).mkString("")

    if (numbers.isEmpty || largest.charAt(0) == '0') "0"
    else largest
  }

  println(largestNumber(List(3, 30, 5, 9, 34)))
  println(largestNumber(List(0, 0, 0, 0, 0)))
  println(largestNumber(List(0)))
  println(largestNumber(List(10, 2)))
  println(largestNumber(List(2020, 20, 1010, 10, 2, 22)))
  println(largestNumber(List(1)))
  println(largestNumber(List()))
}
