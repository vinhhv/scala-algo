package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int

  /**
    *  Medium problems
    */
  def size: Int

  def collectNodes(level: Int): List[BTree[T]]
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing        = throw new NoSuchElementException
  override def left: BTree[Nothing]  = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean      = true

  override def isLeaf: Boolean                     = false
  override def collectLeaves: List[BTree[Nothing]] = List.empty
  override def leafCount                           = 0

  override val size: Int = 0

  override def collectNodes(level: Int): List[BTree[Nothing]] = List.empty
}

case class BNode[+T](
    override val value: T,
    override val left: BTree[T],
    override val right: BTree[T]
) extends BTree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def recurse(queue: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if (queue.isEmpty) leaves
      else if (queue.head.isEmpty) recurse(queue.tail, leaves)
      else if (queue.head.isLeaf) recurse(queue.tail, queue.head :: leaves)
      else recurse(queue.head.left :: queue.head.right :: queue.tail, leaves)
    }

    recurse(List(this), List.empty)
  }

  override def leafCount: Int = collectLeaves.length

  // override def size: Int = {
  //   @tailrec
  //   def recurse(queue: List[BTree[T]], count: Int): Int = {
  //     if (queue.isEmpty) count
  //     else if (queue.head.isEmpty) recurse(queue.tail, count)
  //     else recurse(queue.head.left :: queue.head.right :: queue.tail, count + 1)
  //   }

  //   recurse(List(this), 0)
  // }

  override val size: Int = 1 + left.size + right.size

  override def collectNodes(level: Int): List[BTree[T]] = {
    @tailrec
    def recurse(queue: List[(BTree[T], Int)], acc: List[BTree[T]]): List[BTree[T]] = {
      if (queue.isEmpty) acc
      else if (queue.head._1.isEmpty) recurse(queue.tail, acc)
      else if (queue.head._2 > level) recurse(queue.tail, acc)
      else if (queue.head._2 == level) recurse(queue.tail, queue.head._1 :: acc)
      else {
        val node  = queue.head._1
        val level = queue.head._2
        recurse((node.left, level + 1) :: (node.right, level + 1) :: queue.tail, acc)
      }
    }

    recurse(List((this, 0)), List.empty)
  }
}

object BinaryTreeProblems extends App {
  val tree0 =
    BNode(10, BNode(8, BNode(5, BEnd, BEnd), BNode(9, BEnd, BEnd)), BNode(12, BNode(11, BEnd, BEnd), BEnd))

  println(tree0.isLeaf)        // false
  println(tree0.collectLeaves) // 5, 9, 11
  println(tree0.leafCount)     // 3

  println(tree0.size)

  val degenerate = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
  println(degenerate.size)

  println(tree0.collectNodes(0))        // 10
  println(tree0.collectNodes(0).length) // 10
  println(tree0.collectNodes(1))        // 8, 12
  println(tree0.collectNodes(1).length) // 8, 12
  println(tree0.collectNodes(2))        // 5, 9, 11
  println(tree0.collectNodes(2).length) // 5, 9, 11
}
