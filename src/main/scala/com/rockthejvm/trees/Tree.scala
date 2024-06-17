package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed abstract class Tree[+T] {
  def value: T
  def left: Tree[T]
  def right: Tree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[Tree[T]]
  def leafCount: Int

  /**
    *  Medium problems
    */
  def size: Int

  def collectNodes(level: Int): List[Tree[T]]

  def mirror: Tree[T]

  def sameShapeAs[S >: T](that: Tree[S]): Boolean

  def isSymmetrical: Boolean

  def toList: List[T]
}

case object End extends Tree[Nothing] {
  override def value: Nothing       = throw new NoSuchElementException
  override def left: Tree[Nothing]  = throw new NoSuchElementException
  override def right: Tree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean     = true

  override def isLeaf: Boolean                    = false
  override def collectLeaves: List[Tree[Nothing]] = List.empty
  override def leafCount                          = 0

  override val size: Int = 0

  override def collectNodes(level: Int): List[Tree[Nothing]] = List.empty

  override def mirror: Tree[Nothing] = End

  override def sameShapeAs[S >: Nothing](that: Tree[S]): Boolean = that.isEmpty

  override def isSymmetrical = true

  override def toList: List[Nothing] = List.empty
}

case class Node[+T](
    override val value: T,
    override val left: Tree[T],
    override val right: Tree[T]
) extends Tree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[Tree[T]] = {
    @tailrec
    def recurse(queue: List[Tree[T]], leaves: List[Tree[T]]): List[Tree[T]] = {
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
  //   def recurse(queue: List[Tree[T]], count: Int): Int = {
  //     if (queue.isEmpty) count
  //     else if (queue.head.isEmpty) recurse(queue.tail, count)
  //     else recurse(queue.head.left :: queue.head.right :: queue.tail, count + 1)
  //   }

  //   recurse(List(this), 0)
  // }

  override val size: Int = 1 + left.size + right.size

  // override def collectNodes(level: Int): List[Tree[T]] = {
  //   @tailrec
  //   def recurse(queue: List[(Tree[T], Int)], acc: List[Tree[T]]): List[Tree[T]] = {
  //     if (queue.isEmpty) acc
  //     else if (queue.head._1.isEmpty) recurse(queue.tail, acc)
  //     else if (queue.head._2 > level) recurse(queue.tail, acc)
  //     else if (queue.head._2 == level) recurse(queue.tail, queue.head._1 :: acc)
  //     else {
  //       val node  = queue.head._1
  //       val level = queue.head._2
  //       recurse((node.left, level + 1) :: (node.right, level + 1) :: queue.tail, acc)
  //     }
  //   }

  //   recurse(List((this, 0)), List.empty)
  // }

  override def collectNodes(level: Int): List[Tree[T]] = {
    @tailrec
    def recurse(currentLevel: Int, currentNodes: List[Tree[T]]): List[Tree[T]] = {
      if (currentNodes.isEmpty) List.empty
      else if (currentLevel == level) currentNodes
      else {
        val expandedNodes = for {
          node  <- currentNodes
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child

        recurse(currentLevel + 1, expandedNodes)
      }
    }

    recurse(0, List(this))
  }

  override def mirror: Tree[T] = {
    // def recurse(node: Tree[T]): Tree[T] = {
    //   if (node.isEmpty) End
    //   else if (node.isLeaf) node
    //   else {
    //     val left  = node.left
    //     val right = node.right
    //     Node(node.value, recurse(right), recurse(left))
    //   }
    // }

    // recurse(this)

    @tailrec
    def mirrorTailrec(todo: List[Tree[T]], expanded: Set[Tree[T]], done: List[Tree[T]]): Tree[T] = {
      if (todo.isEmpty) done.head
      else {
        val node = todo.head
        if (node.isEmpty || node.isLeaf) {
          mirrorTailrec(todo.tail, expanded, node :: done)
        } else if (!expanded.contains(node)) {
          mirrorTailrec(node.left :: node.right :: todo, expanded + node, done)
        } else {
          val newLeft  = done.head
          val newRight = done.tail.head
          val newNode  = Node(node.value, newLeft, newRight)
          mirrorTailrec(todo.tail, expanded, newNode :: done.drop(2))
        }
      }
    }

    mirrorTailrec(List(this), Set.empty, List.empty)
  }

  override def sameShapeAs[S >: T](that: Tree[S]): Boolean = {
    @tailrec
    def recurse(queue: List[(Tree[T], Tree[S])]): Boolean = {
      if (queue.isEmpty) true
      else {
        val thisTree = queue.head._1
        val thatTree = queue.head._2

        if (thisTree.isEmpty && thatTree.isEmpty) recurse(queue.tail)
        else if (!thisTree.isEmpty && !thatTree.isEmpty) {
          val leftTrees  = (thisTree.left, thatTree.left)
          val rightTrees = (thisTree.right, thatTree.right)
          recurse(leftTrees :: rightTrees :: queue.tail)
        } else false
      }
    }

    return recurse(List((this, that)))
  }

  override def isSymmetrical: Boolean = sameShapeAs(this.mirror)

  /**
    * Options:
    *
    * pre-order
    * in-order
    * post-order
    * per-level
    */
  override def toList: List[T] = {
    @tailrec
    def recurse(
        queue: List[Tree[T]],
        visited: Set[Tree[T]] = Set.empty[Tree[T]],
        list: List[T] = List.empty[T]
    ): List[T] = {
      if (queue.isEmpty) list.reverse
      else {
        val node = queue.head
        if (node.isEmpty) recurse(queue.tail, visited, list)
        else if (node.isLeaf || visited.contains(node)) recurse(queue.tail, visited, node.value :: list)
        // swap node position to switch between pre-order, in-order and post-order
        else recurse(node.left :: node.right :: node :: queue.tail, visited + node, list)
      }
    }

    @tailrec
    def perLevelR(queue: List[Tree[T]], list: Queue[T] = Queue.empty[T]): List[T] = {
      if (queue.isEmpty) list.toList
      else {
        val node = queue.head
        if (node.isEmpty) perLevelR(queue.tail, list)
        else {
          val newQueue = queue.flatMap(node => List(node.left, node.right).filter(!_.isEmpty))
          val newList  = list ++ queue.map(_.value)
          perLevelR(newQueue, newList)
        }
      }
    }

    // recurse(List(this))
    perLevelR(List(this))
  }
}

object BinaryTreeProblems extends App {
  val tree0 =
    Node(10, Node(8, Node(5, End, End), Node(9, End, End)), Node(12, Node(11, End, End), End))

  println(tree0.isLeaf)        // false
  println(tree0.collectLeaves) // 5, 9, 11
  println(tree0.leafCount)     // 3

  println(tree0.size)

  val degenerate = (1 to 100000).foldLeft[Tree[Int]](End)((tree, number) => Node(number, tree, End))
  println(degenerate.size)

  println(tree0.collectNodes(0))        // 10
  println(tree0.collectNodes(0).length) // 10
  println(tree0.collectNodes(1))        // 8, 12
  println(tree0.collectNodes(1).length) // 8, 12
  println(tree0.collectNodes(2))        // 5, 9, 11
  println(tree0.collectNodes(2).length) // 5, 9, 11
  println(tree0.collectNodes(9))

  println(tree0.mirror)
  println(tree0.sameShapeAs(tree0))        // true
  println(tree0.sameShapeAs(tree0.mirror)) // false

  println(tree0.toList)
}
