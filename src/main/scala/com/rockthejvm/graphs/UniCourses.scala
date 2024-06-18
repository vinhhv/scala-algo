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

    // val dependencies: Graph[Int] =
    //   (0 until nCourses).map(course => (course, Set[Int]())).toMap ++
    //     prerequisites.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

    // (0 until nCourses).forall(course => findCycle(dependencies, course).isEmpty)
  }

  /**
   * 1. Create a dependency graph where given (a, b), we create a map with (b -> Set(a))
   * 2. Create an in-degrees map where course is the key, and the in-degrees count is the value
   * 3. Create a queue with courses that 0 in degrees
   * 4. For every edge from that course to another dependent course, decrement the in-degrees count for that dependent
   * 5. If the new count for that dependent is 0, add it to the queue
   * 6. Add the course to the accumulator
   * 7. Repeat until queue is empty
   *    a. If inDegrees has any values left > 0, there is a cycle and we cannot complete all courses
   *    b. Otherwise, return our accumulator
   */
  def findOrder(nCourses: Int, prerequisites: List[(Int, Int)]): List[Int] = {

    @tailrec
    def recurse(queue: List[Int], dependencies: Graph[Int], inDegrees: Map[Int, Int], acc: List[Int]): List[Int] = {
      if (queue.isEmpty && inDegrees.values.count(_ > 0) > 0) List.empty
      else if (queue.isEmpty) acc.reverse
      else {
        val course = queue.head
        val edges  = dependencies(course)

        val (updatedInDegrees, updatedQueue) = edges.foldLeft((inDegrees, List.empty[Int])) {
          case ((inDegrees, newQueue), edge) =>
            val updatedInDegrees = inDegrees.updatedWith(edge)(_.map(_ - 1))
            if (updatedInDegrees(edge) == 0) (updatedInDegrees, edge :: newQueue)
            else (updatedInDegrees, newQueue)
        }

        recurse(updatedQueue ++ queue.tail, dependencies, updatedInDegrees, course :: acc)
      }
    }

    val dependencies =
      (0 until nCourses).map(course => (course, Set.empty[Int])).toMap ++
        prerequisites.groupBy(_._2).mapValues(_.map(_._1).toSet).toMap

    val inDegrees =
      (0 until nCourses).map(course => (course, 0)).toMap ++
        prerequisites.groupBy(_._1).mapValues(_.map(_._1).toSet.size).toMap

    val queue = inDegrees.filter(_._2 == 0).keySet.toList

    recurse(queue, dependencies, inDegrees, List.empty)
  }

  println(canTakeAllCourses(2, List((0, 1))))                                                 // true
  println(canTakeAllCourses(2, List((0, 1), (1, 0))))                                         // false
  println(canTakeAllCourses(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))                 // true
  println(canTakeAllCourses(5, List((2, 1), (3, 1), (4, 2), (4, 3), (5, 3), (5, 4))))         // true
  println(canTakeAllCourses(5, List((2, 1), (3, 1), (4, 2), (4, 3), (5, 3), (5, 4), (3, 5)))) // false
  println(findOrder(2, List((0, 1))))                                                         // List(1, 0)
  println(findOrder(3, List((0, 1), (1, 2), (2, 0))))                                         // List()
  println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))                         // List(1, 4, 5, 0, 3, 2)
}
