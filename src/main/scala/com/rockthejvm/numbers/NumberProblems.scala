package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberOps {
  extension (n: Int) {
    def isPrime: Boolean = {
      def isPrimeTailrec(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n))) true
        else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
      }

      if (n == 0 || Math.abs(n) == 1) false
      else isPrimeTailrec(2)
    }

    // the constituent prime divisors
    def decompose: List[Int] = {
      assert(n > 0)
      // (2 to Math.sqrt(Math.abs(n)).toInt).toList.flatMap { i =>
      //   if (n % i == 0) {
      //     val iPrime  = if (isPrime(i)) List(i) else List()
      //     val niPrime = if (isPrime(n / i)) List(n / i) else List()
      //     iPrime ++ niPrime
      //   } else List()
      // }

      @tailrec
      def decomposeTailrec(remaining: Int, currentDivisor: Int, acc: List[Int]): List[Int] = {
        if (currentDivisor > Math.sqrt(remaining)) remaining :: acc
        else if (remaining % currentDivisor == 0)
          decomposeTailrec(remaining / currentDivisor, currentDivisor, currentDivisor :: acc)
        else decomposeTailrec(remaining, currentDivisor + 1, acc)
      }

      decomposeTailrec(n, 2, List())
    }

  }
}

object NumberProblems extends App {
  import NumberOps.*

  println(23.isPrime)
  println(64.decompose)
  println(26.decompose)
}
