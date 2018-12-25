package aoc.day05

import scala.io.Source

object AlchemicalReduction extends App {

  val scannedPolymer = Source.fromResource("input_day05.txt").toSeq.mkString

  def react(a: Char, b: Char): Boolean =
    a != b && a.toLower == b.toLower

  def reduce(polymer: String): String = {
    polymer.foldLeft(""){
      case ("", c) => c.toString
      case (s, c) =>
        if (react(s.last, c)) {
          s.slice(0, s.length-1)
        } else {
          s + c
        }
    }
  }

  println(s"After fully reacting the polymer ${reduce(scannedPolymer).length} units remain.")

  val normalizedExistingUnits: Set[Char] = scannedPolymer.map(_.toLower).toSet

  def polymerWithoutUnits(normalizedUnit: Char): String = scannedPolymer.filter(_.toLower != normalizedUnit)

  val shortestPolymerWithoutOneUnit: String = normalizedExistingUnits
    .map(polymerWithoutUnits)
    .map(reduce)
    .minBy(_.length)

  println(s"The length of the shortest polymer you can produce is: ${shortestPolymerWithoutOneUnit.length}")

}
