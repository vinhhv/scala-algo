package com.rockthejvm.strings

import scala.annotation.tailrec

object CompareVersionNumbers extends App {
  def compareVersionNumbers(version1: String, version2: String): Int = {
    def makeLongerVersionNumber(version: String, lengthRequirement: Int): String = {
      val requiredZeroes = lengthRequirement - version.length
      (1 to requiredZeroes).map(_ => '0').mkString + version
    }

    @tailrec
    def compareR(versions1: List[String], versions2: List[String]): Int = {
      if (versions1.isEmpty && versions2.isEmpty) 0
      else {
        // Get version if available
        val v1 = versions1.headOption match {
          case Some(v) => v
          case None    => "0"
        }
        val v2 = versions2.headOption match {
          case Some(v) => v
          case None    => "0"
        }

        // Make versions compatible to compare
        val v1Sized =
          if (v1.length >= v2.length) v1
          else makeLongerVersionNumber(v1, v2.length)
        val v2Sized =
          if (v2.length >= v1.length) v2
          else makeLongerVersionNumber(v2, v1.length)

        val v1Tail =
          if (versions1.isEmpty) List()
          else versions1.tail
        val v2Tail =
          if (versions2.isEmpty) List()
          else versions2.tail

        if (v1Sized == v2Sized) compareR(v1Tail, v2Tail)
        else if (v1Sized > v2Sized) 1
        else -1
      }
    }

    val version1Chunks = version1.split('.').toList
    val version2Chunks = version2.split('.').toList
    compareR(version1Chunks, version2Chunks)
  }

  println(compareVersionNumbers("1", "1"))           // 0
  println(compareVersionNumbers("1.0", "1"))         // 0
  println(compareVersionNumbers("1.0", "1.0.0"))     // 0
  println(compareVersionNumbers("0.9", "1.0.3.4"))   // -1
  println(compareVersionNumbers("1.0.3.4", "1.1.0")) // -1
  println(compareVersionNumbers("1.1.0", "2.0"))     // -1
  println(compareVersionNumbers("2.1", "2.0"))       // 1
  println(compareVersionNumbers("2.1", "2.01"))      // 0
}
