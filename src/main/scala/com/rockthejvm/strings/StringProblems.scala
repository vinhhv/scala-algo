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

  def checkAnagrams(s1: String, s2: String): Boolean = {
    @tailrec
    def recurse(s1: String, s2: String, count: Map[Char, Int]): Boolean = {
      if (s1.isEmpty || s2.isEmpty) {
        count.toList.count(cnt => cnt._2 != 0) == 0
      } else {
        val s1c    = count.getOrElse(s1.head, 0) + 1
        val count1 = count + (s1.head  -> s1c)
        val s2c    = count1.getOrElse(s2.head, 0) - 1
        val count2 = count1 + (s2.head -> s2c)
        recurse(s1.tail, s2.tail, count2)
      }
    }

    recurse(s1, s2, Map.empty[Char, Int])
  }

  println(countCharacters("Scala"))

  println(checkAnagrams("desserts", "stressed"))
  println(checkAnagrams("Scala", "Haskell"))
}
