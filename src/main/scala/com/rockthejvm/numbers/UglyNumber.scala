package com.rockthejvm.numbers

import scala.collection.immutable.Queue

object UglyNumber extends App {

  /**
   * Is ugly if prime decomposition only has 2, 3, 5
   *
    */
  def uglyNumber(number: Int): Boolean = {
    def decomposeTailrec(divisor: Int, remainder: Int, acc: Set[Int]): Set[Int] = {
      if (divisor > Math.sqrt(number)) acc + remainder
      else if (remainder % divisor == 0) decomposeTailrec(divisor, remainder / divisor, acc + divisor)
      else decomposeTailrec(divisor + 1, remainder, acc)
    }

    val primeFactors = decomposeTailrec(2, number, Set.empty[Int])
    val filtered     = primeFactors.filter(i => Set(2, 3, 5).contains(i))
    primeFactors.size == filtered.size
  }

  def nthUgly(n: Int): Int = {
    def min3(a: Int, b: Int, c: Int): Int =
      if (a <= b)
        if (a <= c) a else c
      else if (b <= c) b
      else c

    def nthUglyTailrec(index: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
      val min = min3(q2.head, q3.head, q5.head)
      if (index == n) min
      else {
        val newQ2 = (if (min == q2.head) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if (min == q3.head) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if (min == q5.head) q5.tail else q5).enqueue(min * 5)

        nthUglyTailrec(index + 1, newQ2, newQ3, newQ5)
      }
    }

    if (n == 1) 1
    else nthUglyTailrec(2, Queue(2), Queue(3), Queue(5))
  }

  // ugly numbers
  println(uglyNumber(6))
  println(uglyNumber(25))
  println(uglyNumber(100))
  // non ugly numbers
  println(uglyNumber(14))
  println(uglyNumber(21))

  println("nthUgly")
  println((1 to 100).map(nthUgly).toList)
}
