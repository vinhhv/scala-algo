package com.rockthejvm.graphs

import scala.collection.immutable.Queue
import scala.annotation.tailrec

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice"   -> Set("Bob", "Charlie", "David"),
    "Bob"     -> Set(),
    "Charlie" -> Set("David"),
    "David"   -> Set("Bob", "Vinh", "Mary"),
    "Mary"    -> Set("Bob", "Charlie"),
    "Vinh"    -> Set("Sam"),
    "Sam"     -> Set("Bob", "Vinh", "Mary", "Alice")
  )

  val numbersNetwork: Graph[Char] = Map(
    'A' -> Set('B', 'C', 'D'),
    'B' -> Set('Z'),
    'D' -> Set('J'),
    'J' -> Set('Z')
  )

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    graph.getOrElse(node, Set.empty).size

  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.values.count(_.contains(node))

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    @tailrec
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

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    // @tailrec
    // def recurse(queue: List[T], visited: Set[T], path: List[T]): List[T] = {
    //   if (queue.isEmpty) List.empty
    //   else if (queue.head == end) (path).reverse
    //   else if (visited.contains(queue.head)) recurse(queue.tail, visited, path.tail)
    //   else {
    //     val node           = queue.head
    //     val unvisitedNodes = graph(node).filter(!visited.contains(_))
    //     recurse(unvisitedNodes.toList ++ queue, visited + node, node :: path)
    //   }
    // }

    // recurse(List(start), Set.empty, List.empty)

    @tailrec
    def findPathTailrec(remaining: List[(T, List[T])], visited: Set[T]): List[T] = {
      if (remaining.isEmpty) List.empty
      else {
        val (node, currentPath) = remaining.head
        if (node == end) currentPath.reverse
        else if (visited.contains(node)) findPathTailrec(remaining.tail, visited)
        else {
          val neighbors = graph(node)
          val tuples    = neighbors.map(n => (n, n :: currentPath))
          findPathTailrec(remaining.tail ++ tuples, visited + node)
        }
      }
    }

    findPathTailrec(graph(start).map(n => (n, n :: List(start))).toList, Set.empty)
  }

  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
    // @tailrec
    // def recurse(keys: List[T], newGraph: Graph[T]): Graph[T] = {
    //   if (keys.isEmpty) newGraph
    //   else {
    //     val key                    = keys.head
    //     val neighbors              = graph(key)
    //     val updatedNeighborsForKey = newGraph.getOrElse(key, Set.empty) ++ neighbors

    //     val updatedGraph = neighbors.foldLeft(newGraph) { case (prevGraph, neighbor) =>
    //       val updatedNeighbors = prevGraph.getOrElse(neighbor, Set.empty) + key
    //       prevGraph.updated(neighbor, updatedNeighbors)
    //     }

    //     val finalGraph = updatedGraph.updated(key, updatedNeighborsForKey)
    //     recurse(keys.tail, finalGraph)
    //   }
    // }

    // recurse(graph.keySet.toList, Map())

    def addEdge(graph: Graph[T], from: T, to: T): Graph[T] = {
      val neighbors = graph.getOrElse(from, Set.empty)
      graph.updated(from, neighbors + to)
    }

    @tailrec
    def addOpposingEdges(remainingNodes: Set[T], acc: Graph[T]): Graph[T] = {
      if (remainingNodes.isEmpty) acc
      else {
        val node      = remainingNodes.head
        val neighbors = graph(node)
        val newGraph = neighbors.foldLeft(acc) { case (prevGraph, neighbor) =>
          addEdge(prevGraph, neighbor, node)
        }
        addOpposingEdges(remainingNodes.tail, newGraph)
      }
    }

    addOpposingEdges(graph.keySet, graph)
  }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)

  println(outDegree(socialNetwork, "Alice")) // 3
  println(inDegree(socialNetwork, "David"))  // 2

  println(isPath(socialNetwork, "Alice", "Mary")) // true
  println(isPath(socialNetwork, "Vinh", "Alice")) // false

  println(findPath(socialNetwork, "Alice", "Mary")) // true
  println(findPath(socialNetwork, "Mary", "Alice")) // false

  println(findCycle(socialNetwork, "Alice"))

  println(makeUndirected(socialNetwork))
  println(makeUndirected(socialNetwork)("Bob"))
  println(makeUndirected(numbersNetwork))
}
