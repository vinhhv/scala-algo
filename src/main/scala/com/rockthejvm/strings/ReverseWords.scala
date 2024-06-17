package com.rockthejvm.strings

object ReverseWords extends App {
  // "hello world" => "world hello"
  def reverseWords(string: String): String = {
    string.split("\\s+").toList.reverse.mkString(" ")
  }

  println(reverseWords("hello word"))
  println(reverseWords("Vince loves Scala"))
}
