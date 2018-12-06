package aoc.day06

import scala.io.Source

object ChronalCoordinates extends App {

  sealed trait Coordinate {
    def x: Int
    def y: Int
  }
  case class FieldCoordinate(x: Int, y: Int) extends Coordinate
  case class TargetCoordinate(x: Int, y: Int) extends Coordinate

  case class Distance(fieldC: FieldCoordinate, targetC: TargetCoordinate, manhattan: Int)

  val targets: Seq[TargetCoordinate] = Source.fromResource("input_day06.txt").getLines
    .map(l => {
      val splitString = l.split(", ")
      TargetCoordinate(splitString(0).toInt, splitString(1).toInt)
    })
    .toSeq

  val targetXmin = targets.map(_.x).min
  val targetXmax = targets.map(_.x).max
  val targetYmin = targets.map(_.y).min
  val targetYmax = targets.map(_.y).max

  val fieldPartOne = for {
    x <- targetXmin to targetXmax
    y <- targetYmin to targetYmax
  } yield FieldCoordinate(x, y)

  def manhattanDistance(a: Coordinate)(b: Coordinate): Int =
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)

  def coordsDistances(field: Seq[FieldCoordinate]): Seq[Distance] = {
    for {
      fieldCoord <- field
      inputCoord <- targets
    } yield Distance(fieldCoord, inputCoord, manhattanDistance(fieldCoord)(inputCoord))
  }

  def minimumDistanceTarget(distances: Seq[Distance]): Option[TargetCoordinate] =
    distances.sortBy(_.manhattan).take(2) match {
      case Seq(x) => Some(x.targetC)
      case Seq(x, y) => if (x.manhattan < y.manhattan) Some(x.targetC) else None
    }

  val sizeOfLargestArea: Int =
    coordsDistances(fieldPartOne)
      .groupBy(_.fieldC)
      .mapValues(minimumDistanceTarget)
      .filter(entry => entry._2.isDefined)
      .toSeq
      .groupBy { case (_, targetC) => targetC.get }
      .toSeq
      .sortBy(_._2.length)(Ordering.Int.reverse)
      .head._2.length

  println(s"The size of the largest area is: $sizeOfLargestArea")

  val maxTotalDistance = 10000

  val plus = maxTotalDistance / targets.size

  val fieldPartTwo: Stream[FieldCoordinate] = {
    for {
      x <- (targetXmin - plus to targetXmax + plus).toStream
      y <- (targetYmin - plus to targetYmax + plus).toStream
    } yield FieldCoordinate(x, y)
  }

  def totalDistance(fieldCoordinate: FieldCoordinate): Int =
    targets.map(manhattanDistance(_)(fieldCoordinate)).sum

  val sizeOfRegionWithinDistance: Int =
    fieldPartTwo
      .map(totalDistance)
      .count(_ < maxTotalDistance)

  println(s"The size of the region within a total distance of $maxTotalDistance is: $sizeOfRegionWithinDistance")

}
