package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  // the big 3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
   *  Medium problems
   */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing        = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean     = true

  override def toString(): String = "[]"

  /**
    * Easy problems
    */
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[T](anotherList: RList[T]): RList[T] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil
}

final case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  def isEmpty: Boolean = false

  override def toString(): String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
    * Easy problems
    */
  override def apply(index: Int): T = {
    @tailrec
    def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailRec(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }

  override def length: Int = {
    @tailrec
    def lengthTailRec(remaining: RList[T], count: Int): Int = {
      if (remaining.isEmpty) count
      else lengthTailRec(remaining.tail, count + 1)
    }

    lengthTailRec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remaining: RList[T], reversedList: RList[T]): RList[T] = {
      if (remaining.isEmpty) reversedList
      else reverseTailRec(remaining.tail, remaining.head :: reversedList)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailRec(remaining: RList[S], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else concatTailRec(remaining.tail, remaining.head :: acc)
    }

    concatTailRec(anotherList, this.reverse).reverse
  }

  // Complexity: O(N)
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailrec(remaining: RList[T], currentIndex: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (currentIndex == index) acc.reverse ++ remaining.tail
      else removeAtTailrec(remaining.tail, currentIndex + 1, remaining.head :: acc)
    }

    if (index < 0) throw new NoSuchElementException
    else removeAtTailrec(this, 0, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else mapTailrec(remaining.tail, f(remaining.head) :: acc)
    }

    mapTailrec(this, RNil).reverse
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else flatMapTailrec(remaining.tail, f(remaining.head).reverse ++ acc)
    }

    flatMapTailrec(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailrec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (f(remaining.head)) filterTailrec(remaining.tail, remaining.head :: acc)
      else filterTailrec(remaining.tail, acc)
    }

    filterTailrec(this, RNil)
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailrec(remaining: RList[T], t: T, count: Int, acc: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty) (t, count) :: acc
      else if (remaining.head != t) rleTailrec(remaining.tail, remaining.head, 1, (t, count) :: acc)
      else rleTailrec(remaining.tail, t, count + 1, acc)
    }

    if (this.isEmpty) RNil
    else rleTailrec(this.tail, this.head, 1, RNil).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateEachTailrec(remaining: RList[T], count: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else if (count < k) duplicateEachTailrec(remaining, count + 1, remaining.head :: acc)
      else duplicateEachTailrec(remaining.tail, 0, acc)
    }

    if (k > 0) duplicateEachTailrec(this, 0, RNil).reverse
    else this
  }

  override def rotate(k: Int): RList[T] = {
    @tailrec
    def rotateTailrec(remaining: RList[T], rotationsLeft: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else if (rotationsLeft > 0) rotateTailrec(remaining.tail, rotationsLeft - 1, remaining.head :: acc)
      else remaining ++ acc.reverse
    }

    if (k < 0) this
    else rotateTailrec(this, k % this.length, RNil)
  }

  override def sample(k: Int): RList[T] = {
    val random   = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    @tailrec
    def sampleTailrec(nRemaining: Int, acc: RList[T]): RList[T] = {
      if (nRemaining == 0) acc
      else {
        val index   = random.nextInt(maxIndex)
        val newItem = this(index)
        sampleTailrec(nRemaining - 1, newItem :: acc)
      }
    }
    // def sampleTailrec(remaining: RList[T], count: Int, acc: RList[T]): RList[T] = {
    //   if (count == k) acc
    //   else if (remaining.isEmpty) sampleTailrec(this, count, acc)
    //   else {
    //     val randomNumber = Random.between(0, k)
    //     if (randomNumber == 0) sampleTailrec(remaining.tail, count + 1, remaining.head :: acc)
    //     else sampleTailrec(remaining.tail, count, acc)
    //   }
    // }

    def sampleElegant: RList[T] =
      RList.from((1 to k).map(_ => random.nextInt(maxIndex)).map(index => this(index)))

    // sampleTailrec(this, 0, RNil)
    if (k < 0) RNil
    else sampleTailrec(k, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else convertToRListTailrec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailrec(iterable, RNil)
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil
  val aLargeList = RList.from(1 to 10000)

  def testEasyFunctions() = {
    println(aSmallList)

    // test get-kth
    println(aSmallList.apply(0))
    println(aSmallList.apply(2))
    println(aLargeList.apply(8735))

    // test length
    println(aSmallList.length)
    println(aLargeList.length)

    // test reverse
    println(aSmallList.reverse)
    // println(aLargeList.reverse)

    // test concat
    // println(aSmallList ++ aLargeList)

    // test removeAt
    println(aSmallList.removeAt(1))

    // test map
    println(aSmallList.map(_ * 2))

    // test flatMap
    println(aSmallList.flatMap(i => (i * 2) :: (i * 3) :: RNil))

    // test filter
    println(aSmallList.filter(_ == 2))

  }

  def testMediumFunctions() = {
    val aDuplicatedList = 1 :: 1 :: 1 :: 2 :: 2 :: 3 :: 4 :: 4 :: 5 :: 6 :: 6 :: RNil
    // test rle
    println(aDuplicatedList.rle)

    // test duplicateEach
    println(aSmallList.duplicateEach(3))

    // test rotate
    println("testing rotate")
    println(aSmallList.rotate(0))
    println(aSmallList.rotate(1))
    println(aSmallList.rotate(2))
    println(aSmallList.rotate(3))
    println(aSmallList.rotate(4))

    // test sample
    println("test sample")
    println(aSmallList.sample(10))
  }

  testMediumFunctions()
}
