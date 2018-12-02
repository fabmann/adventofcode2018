package aoc.day01

import scala.annotation.tailrec
import scala.io.Source

object ChronalCalibration extends App {

  val frequencyChanges: Seq[Int] = Source.fromResource("input_day01.txt")
    .getLines
    .map(l => l.toInt)
    .toSeq

  private val solutionPartOne: Int = resultingFrequency(0, frequencyChanges)
  println(s"The resulting frequency after applying the input changes is: $solutionPartOne")
  private val solutionPartTwo: Int = firstFrequencyReachedTwice(0, frequencyChanges)
  println(s"The first recurring frequency after infinitely applying the changes is: $solutionPartTwo")

  @tailrec
  def resultingFrequency(startingFrequency: Int, frequencyChanges: Seq[Int]): Int = {
    if (frequencyChanges.isEmpty) {
      startingFrequency
    } else {
      resultingFrequency(startingFrequency + frequencyChanges.head, frequencyChanges.tail)
    }
  }

  def firstFrequencyReachedTwice(startingFrequency: Int, frequencyChanges: Seq[Int]): Int = {
    toInfiniteStream(frequencyChanges)
      .scanLeft(
        (Set.empty: Set[Int], startingFrequency)
      ){
        case ((seenFreqs, curFreq), change) => (seenFreqs + curFreq, curFreq + change)
      }
      .find{case (seenFreqs,curFreq) => seenFreqs.contains(curFreq)}
      .get._2
  }

  def toInfiniteStream(changes: Seq[Int]): Stream[Int] = {
    def loop(stream: Stream[Int]): Stream[Int] = {
      if (stream.isEmpty) {
        loop(changes.toStream)
      } else {
        stream.head #:: loop(stream.tail)
      }
    }
    loop(changes.toStream)
  }

}

