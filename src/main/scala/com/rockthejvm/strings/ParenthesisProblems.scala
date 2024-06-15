package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  def hasValidParentheses(string: String): Boolean = {
    @tailrec
    def recurse(remaining: String, openCount: Int): Boolean = {
      if (remaining.isEmpty) openCount == 0
      else {
        remaining.head match {
          case '(' => recurse(remaining.tail, openCount + 1)
          case ')' if openCount > 0 =>
            recurse(remaining.tail, openCount - 1)
          case _ => false
        }
      }
    }

    recurse(string, 0)
  }

  final case class Parens(parens: String, openCount: Int, openLeft: Int)

  // Complexity: O(2^n)
  def generateAllValidParentheses(n: Int): List[String] = {
    @tailrec
    def recurse(remaining: List[Parens], acc: List[String]): List[String] = {
      if (remaining.isEmpty) acc
      else {
        val p = remaining.head
        // Keep opening parens if openLeft
        val openParens  = if (p.openLeft > 0) List(Parens(p.parens + '(', p.openCount + 1, p.openLeft - 1)) else List()
        val closeParens = if (p.openCount > 0) List(Parens(p.parens + ')', p.openCount - 1, p.openLeft)) else List()

        if (openParens.isEmpty && closeParens.isEmpty)
          recurse(remaining.tail ++ openParens ++ closeParens, acc :+ p.parens)
        else recurse(remaining.tail ++ openParens ++ closeParens, acc)
      }
    }

    if (n <= 0) List.empty[String]
    else recurse(List(Parens("(", 1, n - 1)), List())
  }

  println(hasValidParentheses("()()()()(((())))"))
  println(hasValidParentheses("())()()()(((())))"))

  println(generateAllValidParentheses(0))
  println(generateAllValidParentheses(1))
  println(generateAllValidParentheses(2))
  println(generateAllValidParentheses(3))
  println(generateAllValidParentheses(4))
  println(!generateAllValidParentheses(0).map(hasValidParentheses).contains(false))
  println(!generateAllValidParentheses(1).map(hasValidParentheses).contains(false))
  println(!generateAllValidParentheses(2).map(hasValidParentheses).contains(false))
  println(!generateAllValidParentheses(3).map(hasValidParentheses).contains(false))
  println(!generateAllValidParentheses(4).map(hasValidParentheses).contains(false))
}
