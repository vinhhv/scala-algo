package com.rockthejvm.numbers

object Duplicates extends App {
  def duplicates(list: List[Int]): Int = {
    def duplicatesTailrec(remaining: List[Int], mask: Int): Int = {
      if (remaining.isEmpty) mask
      else duplicatesTailrec(remaining.tail, mask ^ remaining.head)
    }

    duplicatesTailrec(list, 0)
  }

  println(duplicates(List(1, 2, 3, 4, 5, 1, 2, 3, 4, 6, 7, 8, 9, 6, 7, 8, 9)))
}
