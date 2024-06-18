package com.rockthejvm.graphs

import scala.annotation.tailrec
import com.rockthejvm.graphs.GraphProblems.findCycle

object UniCourses extends App {

  /**
    * nCourses courses at Uni, labels 0 -> n -1
    * prerequisites = List[(a, b)]
    * (a, b) = b is required to order to take a
    * 
    * Can you take all courses 0 ..  n - 1 without breaking any prerequisites?
    */
  def canTakeAllCourses(nCourses: Int, prerequisites: List[(Int, Int)]): Boolean = {
    @tailrec
    def traverse(queue: List[(Int, Int)], visited: Map[Int, Int], dependencies: Graph[Int]): Boolean = {
      // If the index stored in visited is equivalent to the one from the queue, we are backtracking
      def isCycle(course: Int, index: Int): Boolean = visited.getOrElse(course, -1) == index

      // If the index stored in visited is less than the one from the queue, we found a cycle
      def isBacktracking(course: Int, index: Int): Boolean = visited.getOrElse(course, Int.MaxValue) < index

      def getDependentsWithIndex(course: Int, lastIndex: Int): List[(Int, Int)] =
        dependencies.getOrElse(course, Set()).toList.foldRight(List.empty[(Int, Int)]) { (dependent, ds) =>
          (dependent, ds.headOption.getOrElse((-1, lastIndex))._2 + 1) :: ds
        }

      if (queue.isEmpty) true
      else {
        val (course, index) = queue.head
        if (isCycle(course, index)) traverse(queue.tail, visited - course, dependencies)
        else if (isBacktracking(course, index)) false
        else {
          val dependents = getDependentsWithIndex(course, index)
          traverse(dependents ++ queue, visited.updated(course, index), dependencies)
        }
      }
    }

    val dependencies: Graph[Int] =
      (0 until nCourses).map(course => (course, Set[Int]())).toMap ++
        prerequisites.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

    val queue   = dependencies.keySet.toList.zipWithIndex.reverse
    val visited = queue.toMap

    traverse(queue, Map.empty, dependencies)
  }

  // val dependencies: Graph[Int] =
  //   (0 until nCourses).map(course => (course, Set[Int]())).toMap ++
  //     prerequisites.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

  // (0 until nCourses).forall(course => findCycle(dependencies, course).isEmpty)
  println(canTakeAllCourses(2, List((0, 1))))                                                 // true
  println(canTakeAllCourses(2, List((0, 1), (1, 0))))                                         // false
  println(canTakeAllCourses(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))                 // true
  println(canTakeAllCourses(5, List((2, 1), (3, 1), (4, 2), (4, 3), (5, 3), (5, 4))))         // true
  println(canTakeAllCourses(5, List((2, 1), (3, 1), (4, 2), (4, 3), (5, 3), (5, 4), (3, 5)))) // false
  // println(findOrder(2, List((0, 1))))                                         // List(1, 0)
  // println(findOrder(3, List((0, 1), (1, 2), (2, 0))))                         // List()
  // println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))         // List(1, 4, 5, 0, 3, 2)
}
