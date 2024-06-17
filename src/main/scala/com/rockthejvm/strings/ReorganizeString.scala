package com.rockthejvm.strings

import scala.collection.immutable.SortedSet
import scala.annotation.tailrec

object ReorganizeString extends App {
  given ordering: Ordering[(Char, Int)] = new Ordering[(Char, Int)] {
    def compare(x: (Char, Int), y: (Char, Int)): Int = {
      val primary = y._2 compare x._2
      if (primary != 0) primary else x._1 compare y._1
    }
  }

  def reorganizeString(string: String): String = {
    def getCountTuples(string: String): List[(Char, Int)] = {
      string.groupBy(c => c).view.mapValues(_.length).toList
    }

    @tailrec
    def reorganize(queue: SortedSet[(Char, Int)], lastChar: Option[(Char, Int)], acc: String): String = {
      if (acc.length == string.length) acc
      else if (queue.isEmpty) ""
      else {
        val newCharCount = queue.head
        val newChar      = newCharCount._1
        val newCount     = newCharCount._2 - 1

        val newQueue = lastChar match {
          case Some(charCount) => queue.tail + charCount
          case None            => queue.tail
        }

        if (newCount == 0) reorganize(newQueue, None, acc + newChar)
        else reorganize(newQueue, Some((newChar, newCount)), acc + newChar)
      }
    }

    val countTuples = getCountTuples(string)
    val queue       = SortedSet(countTuples*)

    reorganize(queue, None, "")
  }

  println(reorganizeString("aaabc"))
}
