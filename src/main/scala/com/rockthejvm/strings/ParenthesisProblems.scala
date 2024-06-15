package com.rockthejvm.strings

object ParenthesisProblems extends App {

  def hasValidParentheses(string: String): Boolean = {
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

  println(hasValidParentheses("()()()()(((())))"))
  println(hasValidParentheses("())()()()(((())))"))
}
