package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersTailRec(s: String, count: Map[Char, Int]): Map[Char, Int] = {
      if (s.isEmpty()) count
      else
        countCharactersTailRec(
          s.tail, {
            val cCount = count.getOrElse(s.head, 0) + 1
            count + (s.head -> cCount)
          }
        )
    }

    countCharactersTailRec(s, Map.empty[Char, Int])
  }

  println(countCharacters("Scala"))
}
