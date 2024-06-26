package com.rockthejvm.graphs

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

object NetworkDelays {
  /*
   * n = number of nodes
   * times = list((a, b, t))
   * (a,b,t) = time between node a and node b is t
   *
   * What is the minimum time it takes for the signal to go from the source
   * to ALL the other nodes in the network?
   *
   * 1. Create a graph from node to edges from node
   * (ex. {
   *        1: [(2, 3), (3, 10), (4, 10)]
   *        2: [(3, 4)]
   *        ...
   *      }
   * )
   *
   * 2. Starting from node n, traverse greedily until all nodes are visited
   * 3. We choose which nodes based on a heap of edges, where edges are sorted by
   *    SHORTEST TOTAL DISTANCE from node n
   * 4. We keep track of max total distance from heap and return that once we visit all nodes
   */

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    val newTimes: List[(Int, Int, Int)] = times.toList
      .map(_.toList)
      .map { time =>
        time match {
          case a :: b :: t :: tail => (a, b, t)
          case _                   => throw new IllegalArgumentException
        }
      }
      .toList

    computerNetworkDelay(k, newTimes, n)
  }

  def computerNetworkDelay(n: Int, times: List[(Int, Int, Int)], k: Int = 0): Int = {
    @tailrec
    def recurse(
        pq: PriorityQueue[(Int, Int)],
        graph: Map[Int, List[(Int, Int)]],
        visited: Set[Int],
        maxDistance: Int
    ): Int = {
      if (pq.isEmpty && visited.size == k) maxDistance
      else if (pq.isEmpty) -1
      else if (visited.contains(pq.head._1)) recurse(pq.drop(1), graph, visited, maxDistance)
      else {
        val (node, totalDistanceSoFar) = pq.dequeue()
        val edges                      = graph.getOrElse(node, List.empty)

        val newMaxDistance = math.max(maxDistance, totalDistanceSoFar)
        val newPQ = edges.map { (destination, time) =>
          pq.enqueue((destination, totalDistanceSoFar + time))
        }
        recurse(pq, graph, visited + node, newMaxDistance)
      }
    }

    val graph = times.foldLeft(Map.empty[Int, List[(Int, Int)]]) { case (graph, (source, destination, time)) =>
      val edges = (destination, time) :: graph.getOrElse(source, List.empty)
      graph.updated(source, edges)
    }

    val orderingByTime: Ordering[(Int, Int)] = Ordering.by(-_._2)
    val pq                                   = PriorityQueue(graph.getOrElse(n, List.empty)*)(orderingByTime)
    recurse(pq, graph, Set(n), 0)
  }

  def main(args: Array[String]): Unit = {
    val times       = List((1, 2, 3), (2, 3, 4), (3, 4, 2), (1, 3, 10), (1, 4, 10)) // 9
    val brokenTimes = List((1, 2, 3), (2, 3, 4), (1, 3, 10))                        // -1
    println(computerNetworkDelay(1, times, 4))
    println(computerNetworkDelay(1, brokenTimes, 4))

    val lc1 = Array(Array(2, 1, 1), Array(2, 3, 1), Array(3, 4, 1))
    val lc2 = Array
  }
}
