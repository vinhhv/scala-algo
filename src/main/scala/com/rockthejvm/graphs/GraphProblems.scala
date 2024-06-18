package com.rockthejvm.graphs

import scala.collection.immutable.Queue

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice"   -> Set("Bob", "Charlie", "David"),
    "Bob"     -> Set(),
    "Charlie" -> Set("David"),
    "David"   -> Set("Bob", "Mary"),
    "Mary"    -> Set("Bob", "Charlie"),
    "Vinh"    -> Set("Sam"),
    "Sam"     -> Set("Vinh")
  )

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    graph.getOrElse(node, Set.empty).size

  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.values.count(_.contains(node))

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    def recurse(queue: Queue[T], visited: Set[T]): Boolean = {
      if (queue.isEmpty) false
      else if (queue.head == end) true
      else {
        val (key, newQueue) = queue.dequeue
        if (graph.contains(key)) {
          val nodes         = graph(key)
          val filteredNodes = nodes.filter(!visited.contains(_))
          val updatedQueue  = newQueue.enqueueAll(filteredNodes)
          recurse(updatedQueue, visited ++ filteredNodes)
        } else recurse(newQueue, visited + key)
      }
    }

    recurse(Queue(start), Set.empty)
  }

  println(outDegree(socialNetwork, "Alice")) // 3
  println(inDegree(socialNetwork, "David"))  // 2

  println(isPath(socialNetwork, "Alice", "Mary")) // true
  println(isPath(socialNetwork, "Mary", "Alice")) // false
  println(isPath(socialNetwork, "Vinh", "Alice")) // false
}
