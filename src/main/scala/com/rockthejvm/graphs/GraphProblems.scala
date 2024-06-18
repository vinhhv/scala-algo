package com.rockthejvm.graphs

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice"   -> Set("Bob", "Charlie", "David"),
    "Bob"     -> Set(),
    "Charlie" -> Set("David"),
    "David"   -> Set("Bob", "Mary"),
    "Mary"    -> Set("Bob", "Charlie")
  )

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    graph.getOrElse(node, Set.empty).size

  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.values.count(_.contains(node))

  println(outDegree(socialNetwork, "Alice")) // 3
  println(inDegree(socialNetwork, "David"))  // 2
}
