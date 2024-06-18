package com.rockthejvm.graphs

import scala.annotation.tailrec

object TownJudge extends App {

  /**
    * n people, 1 to n
    * trust = List[(a, b)]
    * (a, b) = a trusts b
    * 
    * There might be a town judge.
    *   The town judge trusts nobody.
    *   Everybody (except for the judge) trusts the town judge
    *   There is exactly on person that satisfies these properties
    * 
    * Find the town judge, or return -1
    *
    */
  def findJudge(n: Int, trust: List[(Int, Int)]): Int = {
    @tailrec
    def recurse(potentialJudges: List[Int], trustGraph: Map[Int, Set[Int]]): Int = {
      if (potentialJudges.isEmpty) -1
      else if (trustGraph.keySet.contains(potentialJudges.head)) recurse(potentialJudges.tail, trustGraph)
      else potentialJudges.head
    }

    val inDegrees: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()) {
      { case (map, (_, b)) =>
        map + (b -> (map.getOrElse(b, 0) + 1))
      }
    }

    val outDegrees: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()) {
      { case (map, (a, _)) =>
        map + (a -> (map.getOrElse(a, 0) + 1))
      }
    }

    val townJudgeOption: Option[Int] = (1 to n).find { person =>
      inDegrees.getOrElse(person, 0) == n - 1 && outDegrees.getOrElse(person, 0) == 0
    }

    val trustGraph = trust.foldLeft[Map[Int, Set[Int]]](Map()) { case (graph, (a, b)) =>
      val trusts = graph.getOrElse(a, Set()) + b
      graph.updated(a, trusts)
    }
    val trustedByGraph = trust.foldLeft[Map[Int, Set[Int]]](Map()) { case (graph, (a, b)) =>
      val trustees = graph.getOrElse(b, Set()) + a
      graph.updated(b, trustees)
    }

    val potentialJudges = trustedByGraph.filter(t => t._2.size == n - 1).keySet.toList
    recurse(potentialJudges, trustGraph)

    townJudgeOption.getOrElse(-1)
  }

  println(findJudge(2, List((1, 2))))                                 // 2
  println(findJudge(3, List((1, 2), (3, 2))))                         // 2
  println(findJudge(3, List((1, 2), (2, 3), (3, 1))))                 // -1
  println(findJudge(4, List((1, 3), (2, 3), (1, 4), (2, 4), (3, 4)))) // 4
}
