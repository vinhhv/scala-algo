package com.rockthejvm.trees

import scala.annotation.tailrec

object PathSum extends App {

  // Return true if there is a path from root to a leaf,
  // such that the sum of the values is target.
  def hasPathSum(tree: Tree[Int], target: Int): Boolean = {
    def stackR(node: Tree[Int], sum: Int = 0): Boolean = {
      if (node.isEmpty) false
      else if (node.isLeaf && sum + node.value == target) true
      else stackR(node.left, sum + node.value) || stackR(node.right, sum + node.value)
    }

    @tailrec
    def tailR(queue: List[(Tree[Int], Int)]): Boolean = {
      if (queue.isEmpty) false
      else if (queue.head._1.isEmpty) tailR(queue.tail)
      else {
        val node    = queue.head._1
        val nodeSum = queue.head._2 + node.value
        if (node.isLeaf && nodeSum == target) true
        else tailR((node.left, nodeSum) :: (node.right, nodeSum) :: queue.tail)
      }
    }

    if (tree.isEmpty) true
    else tailR(List((tree, 0)))
    // else stackR(tree)
  }

  val tree =
    Node(1, Node(2, Node(3, End, End), Node(4, End, Node(5, End, End))), Node(6, Node(7, End, End), Node(8, End, End)))

  println(hasPathSum(tree, 6))  // true
  println(hasPathSum(tree, 7))  // false
  println(hasPathSum(tree, 14)) // true
  println(hasPathSum(tree, 15)) // true
  println(hasPathSum(tree, 16)) // false
  println(hasPathSum(End, 0))   // true?

  val twoPathsto6 =
    Node(1, Node(2, Node(3, End, End), Node(4, End, Node(-1, End, End))), Node(6, Node(7, End, End), Node(8, End, End)))

  // println(findSumPaths(twoPathsto6, 6))
  // println(findSumPaths(twoPathsto6, 7))
}
