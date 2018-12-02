package aoc.day02

import scala.io.Source

object InventoryManagementSystem extends App {

  val inputBoxIds = Source.fromResource("input_day02.txt")
    .getLines
    .toSeq

  val inputLetterCounts = inputBoxIds
    .map(letterCount)

  val checksum = inputLetterCounts.count(countsFor(2)) * inputLetterCounts.count(countsFor(3))

  println(s"The checksum is: $checksum")

  println(s"The common letters are: ${commonLetters(inputBoxIds)}")

  def letterCount(boxId: String): Map[Char, Int] = {
    boxId.groupBy(c => c).mapValues(_.length)
  }

  def countsFor(n: Int)(letterCount: Map[Char, Int]): Boolean = {
    letterCount.values.toSet.contains(n)
  }

  def commonLetters(boxIds: Seq[String]): String = {
    val crossProduct = for {
      a <- boxIds
      b <- boxIds
  } yield (a, b)

    crossProduct
      .filter(Function.tupled(_ != _))
      .map(Function.tupled(commonChars))
      .sortBy(_.length)(Ordering.Int.reverse)
      .head
  }

  def commonChars(a: String, b: String): String = {
    a.zip(b)
      .filter(c => c._1 == c._2)
      .map(_._1)
      .foldLeft("")(_ + _)
  }

}
