package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersTailRec(s: String, count: Map[Char, Int]): Map[Char, Int] = {
      if (s.isEmpty()) count
      else
        countCharactersTailRec(
          s.tail, {
            val cCount = count.getOrElse(s.head, 0) + 1
            count + (s.head -> cCount)
          }
        )
    }

    countCharactersTailRec(s, Map.empty[Char, Int])
  }

  def checkAnagrams(s1: String, s2: String): Boolean = {
    @tailrec
    def recurse(s1: String, s2: String, count: Map[Char, Int]): Boolean = {
      if (s1.isEmpty || s2.isEmpty) {
        count.toList.count(cnt => cnt._2 != 0) == 0
      } else {
        val s1c    = count.getOrElse(s1.head, 0) + 1
        val count1 = count + (s1.head  -> s1c)
        val s2c    = count1.getOrElse(s2.head, 0) - 1
        val count2 = count1 + (s2.head -> s2c)
        recurse(s1.tail, s2.tail, count2)
      }
    }

    recurse(s1, s2, Map.empty[Char, Int])
  }

  final case class Line(words: List[String], minLength: Int, wordsLength: Int)
  object Line {
    def empty: Line = Line(List(), 0, 0)
  }

  def justify(text: String, width: Int): String = {
    def makeText(line: Line): String = {
      @tailrec
      def makeTextTailrec(
          words: List[String],
          acc: String,
          spaces: List[String],
          firstWord: Boolean
      ): String = {
        if (words.isEmpty) acc + '\n'
        else if (words.tail.isEmpty) {
          if (firstWord) makeTextTailrec(words.tail, acc + words.head, spaces, false)
          else makeTextTailrec(words.tail, acc + spaces.head + words.head, spaces.tail, false)
        } else {
          if (firstWord) makeTextTailrec(words.tail, acc + words.head, spaces, false)
          else makeTextTailrec(words.tail, acc + spaces.head + words.head, spaces.tail, false)
        }
      }

      @tailrec
      def makeEvenSpaces(spaces: List[String], spacesLeft: Int, acc: List[String]): List[String] = {
        if (spacesLeft == 0) acc ++ spaces
        else if (spaces.isEmpty) makeEvenSpaces(acc, spacesLeft, List())
        else makeEvenSpaces(spaces.tail, spacesLeft - 1, acc :+ (spaces.head + ' '))
      }

      val numSpaceChunks = line.words.length - 1
      val spacesNeeded   = width - line.wordsLength
      val initialSpaces  = (1 to numSpaceChunks).map(_ => " ").toList
      val spaces         = makeEvenSpaces(initialSpaces, spacesNeeded - numSpaceChunks, List())
      makeTextTailrec(line.words, "", spaces, true)
    }

    @tailrec
    def recurse(allWords: List[String], line: Line, finalText: String): String = {
      if (allWords.isEmpty) finalText
      else {
        val word        = allWords.head
        val space       = if (line.words.isEmpty) 0 else 1
        val minLength   = line.minLength + word.length + space
        val wordsLength = line.wordsLength + word.length
        if (minLength > width) recurse(allWords, Line.empty, finalText + makeText(line))
        else recurse(allWords.tail, line.copy(line.words :+ word, minLength, wordsLength), finalText)
      }
    }

    val wordChunks = text.split("\\s+").toList
    recurse(wordChunks, Line.empty, "")
  }

  println(countCharacters("Scala"))

  println(checkAnagrams("desserts", "stressed"))
  println(checkAnagrams("Scala", "Haskell"))

  println(justify("Scala is the most amazing language you will ever write any code in", 16))
  println(
    justify(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam vel urna bibendum, pharetra mi quis, imperdiet nibh. Praesent dictum odio lacus, eget commodo sem aliquam rutrum. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec eget rhoncus mauris, quis vehicula mi. Quisque finibus purus non varius dictum. Pellentesque vulputate fringilla egestas. Nunc eleifend ex sed egestas cursus. Praesent molestie nisl in pretium vehicula. Vestibulum efficitur ut risus quis porta. Praesent non sem quam. Donec vitae arcu sapien. Quisque aliquet nibh in metus efficitur ullamcorper. Donec mattis dapibus nisl sed iaculis. Curabitur eu blandit enim. Fusce varius.",
      80
    )
  )
  println(
    justify(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc vestibulum gravida justo, id luctus nulla dapibus ut. Ut elementum ac metus at vestibulum. Quisque pellentesque id nisi sed efficitur. Maecenas consectetur diam ac orci convallis mollis. Etiam sem purus, accumsan consequat mattis at, gravida at justo. Nullam molestie ex non cursus semper. Cras porta nunc sed tempus finibus. Maecenas pretium nibh est, id scelerisque turpis convallis vel. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam erat volutpat. Duis sed convallis augue. Integer lacus eros, posuere ut metus at, vestibulum consectetur lorem.\n\nPhasellus vel pulvinar sem, a rutrum lectus. Donec quis nisi nec leo faucibus sodales id non nunc. Fusce gravida diam vitae orci sollicitudin, a efficitur urna fermentum. Vivamus gravida ante sed lectus ornare, eget egestas leo porta. Curabitur sed blandit metus. Ut at augue consequat, suscipit nulla eu, euismod tellus. Mauris eu mi faucibus, elementum ligula sed, placerat lectus. Morbi varius magna eu mauris ultricies, vitae blandit purus pharetra. Etiam finibus non odio in eleifend. Cras viverra, dolor at ullamcorper pharetra, ligula nunc mattis est, ut volutpat odio dui vitae ligula. Nulla vitae tincidunt arcu. Nulla facilisi. Duis ullamcorper eros neque, at tincidunt.",
      100
    )
  )
}
