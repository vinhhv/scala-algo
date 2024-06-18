package com.rockthejvm.trees

import scala.annotation.tailrec

object CameraSurveillance {
  /*
    Given a binary tree, we install cameras on the nodes of the tree.
    Each camera at a node can monitor its parent, itself, and its immediate children.
    Calculate the minimum number of cameras needed to monitor all nodes of the tree.

          1
         / \
        2   3
       /
      4
      => 2

              ______1______ <--
             /             \
           __2__         __3__
          /     \       /     \
         >4    >5       6<    7<
         / \     \     / \     \
       >8   9    10   11  12<  13
       /                   \
      14                   15

      => 7

              ______1<_____
             /             \
           __2__         __3__
          /     \       /     \
          4<    5<      6<    7<
         / \     \     / \     \
        8   9    10   11  12<  13
       /                   \
      14<                  15
     /
    17

    A leaf NEVER needs a camera, because it can be covered by its parent, who could also cover two more spots
    A node with at least one leaf MUST have a camera

    A node with another one node
    What about nodes with two leaves?
   */
  def cameraSurveillance[T](tree: Tree[T]): Int = {
    val COVERED     = 0
    val NOT_COVERED = 1
    val CAMERA      = 2

    def minCamerasStack(node: Tree[T]): (Int, Int) =
      if (node.isEmpty) (0, COVERED)
      else {
        val (leftNumCameras, leftState)   = minCamerasStack(node.left)
        val (rightNumCameras, rightState) = minCamerasStack(node.right)

        /*
         * - left or right is NOT covered => place camera in this node
         * - left or right HAVE CAMERAS => consider the NODE covered
         * - consider the node NOT_COVERED
         */
        if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
        else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
        else (leftNumCameras + rightNumCameras, NOT_COVERED)
      }

    def minCamerasTail(stack: List[Tree[T]], visited: Set[Tree[T]], coverageStack: List[(Int, Int)]): (Int, Int) = {
      if (stack.isEmpty) coverageStack.head
      else if (stack.head.isEmpty) minCamerasTail(stack.tail, visited, (0, COVERED) :: coverageStack)
      else {
        val node = stack.head

        if (node.isLeaf) minCamerasTail(stack.tail, visited, (0, NOT_COVERED) :: coverageStack)
        else if (visited.contains(node)) {
          val (leftNumCameras, leftState)   = coverageStack.head
          val (rightNumCameras, rightState) = coverageStack.tail.head

          val nextCoverage =
            if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
            else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
            else (leftNumCameras + rightNumCameras, NOT_COVERED)

          minCamerasTail(stack.tail, visited, nextCoverage :: coverageStack.drop(2))
        } else {
          minCamerasTail(node.left :: node.right :: stack, visited + node, coverageStack)
        }
      }
    }

    val (stackNumCameras, stackRootState) = minCamerasTail(List(tree), Set(), List())
    if (stackRootState == NOT_COVERED) stackNumCameras + 1
    else stackNumCameras
  }

  def main(args: Array[String]): Unit = {
    val smallTree =
      Tree(1, Tree(2, Tree(4), Tree()), Tree(3))

    val biggerTree =
      Tree(
        1,
        Tree(2, Tree(4, Tree(8, Tree(14), Tree()), Tree(9)), Tree(5, Tree(), Tree(10))),
        Tree(3, Tree(6, Tree(11), Tree(12, Tree(), Tree(15))), Tree(7, Tree(), Tree(13)))
      )

    println(cameraSurveillance(Tree()))     // 0
    println(cameraSurveillance(Tree(1)))    // 1
    println(cameraSurveillance(smallTree))  // 2
    println(cameraSurveillance(biggerTree)) // 7
  }
}
