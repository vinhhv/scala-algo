package com.rockthejvm.strings

import scala.annotation.tailrec

object RansomNote extends App {
  // def ransomNote(note: String, magazine: String): Boolean = {
  //   @tailrec
  //   def getCharCount(remainingNote: String, countMap: Map[Char, Int]): Map[Char, Int] = {
  //     if (remainingNote.isEmpty) countMap
  //     else {
  //       val newCount = countMap.getOrElse(remainingNote.head, 0) + 1
  //       val newMap   = countMap + (remainingNote.head -> newCount)
  //       getCharCount(remainingNote.tail, newMap)
  //     }
  //   }

  //   @tailrec
  //   def canRansom(remainingMagazine: String, charCountMap: Map[Char, Int]): Boolean = {
  //     if (charCountMap.isEmpty) true
  //     else if (remainingMagazine.isEmpty) false
  //     else if (charCountMap.contains(remainingMagazine.head)) {
  //       val newCount = charCountMap.getOrElse(remainingMagazine.head, 0) - 1
  //       if (newCount == 0) canRansom(remainingMagazine.tail, charCountMap.removed(remainingMagazine.head))
  //       else canRansom(remainingMagazine.tail, charCountMap + (remainingMagazine.head -> newCount))
  //     } else canRansom(remainingMagazine.tail, charCountMap)
  //   }

  //   val charCountMap = getCharCount(note, Map())
  //   canRansom(magazine, charCountMap)

  def ransomNote(note: String, magazine: String): Boolean = {
    def buildCountMap(string: String): Map[Char, Int] =
      string.groupBy(c => c).view.mapValues(_.length).toMap
      // string.foldLeft(Map[Char, Int]()) { case (map, char) =>
      //   map + (char -> (map.getOrElse(char, 0) + 1))
      // }

    val noteMap     = buildCountMap(note)
    val magazineMap = buildCountMap(magazine)
    noteMap.keySet.forall(char => noteMap.getOrElse(char, 0) <= magazineMap.getOrElse(char, 0))
  }

  println(ransomNote("Hello, my name is Vince", "alkal;sdfja;lsdkjfaldsk;fja;ldsfkjadafkl;aad;klsfj"))

  println(
    ransomNote(
      "I have your daughter. I want 1000000 dollars, or you'll never see her again.",
      "I bought this really nice doll for my daughter. It was 20 dollars on Amazon. She's never been happier. I often have discounts from my network, so if you want to buy some really cool stuff for your kids, I can send you an invite if you sign up to my newsletter. It's read by 100000 people, and you'll never need to search for online discounts again."
    )
  )
}
