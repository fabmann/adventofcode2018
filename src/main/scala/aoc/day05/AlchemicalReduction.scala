package aoc.day05

import scala.annotation.tailrec
import scala.io.Source

object AlchemicalReduction extends App {

  val scannedPolymer = Source.fromResource("input_day05.txt").toSeq.mkString

  def react(a: Char, b: Char): Boolean =
    a.toLower == b.toLower && ((a.isLower && b.isUpper) || (a.isUpper && b.isLower))

  @tailrec
  def reductionStep(acc: String, polymer: String): String = {
    if (polymer.isEmpty) {
      acc
    } else if (polymer.length == 1) {
      acc + polymer
    } else {
      if (react(polymer.head, polymer.tail.head)){
        reductionStep(acc, polymer.tail.tail)
      } else {
        reductionStep(acc + polymer.head, polymer.tail)
      }
    }
  }

  @tailrec
  def reduceToMinimum(polymer: String): String = {
    val reducedPoloymer = reductionStep("", polymer)
    if (reducedPoloymer.length == polymer.length) {
      reducedPoloymer
    } else {
      reduceToMinimum(reducedPoloymer)
    }
  }

  println(s"After fully reacting the polymer ${reduceToMinimum(scannedPolymer).length} units remain.")

  val normalizedExistingUnits: Set[Char] = scannedPolymer.map(_.toLower).toSet

  def polymerWithoutUnits(normalizedUnit: Char): String = scannedPolymer.filter(_.toLower != normalizedUnit)

  val shortestPolymerWithoutOneUnit: String = normalizedExistingUnits
    .map(polymerWithoutUnits)
    .map(reduceToMinimum)
    .minBy(_.length)

  println(s"The length of the shortest polymer you can produce is: ${shortestPolymerWithoutOneUnit.length}")

}
