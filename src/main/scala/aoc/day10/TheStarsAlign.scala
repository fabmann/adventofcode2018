package aoc.day10

import scala.annotation.tailrec
import scala.io.Source

object TheStarsAlign extends App {

  case class Star(x: Int, y: Int, dx: Int, dy: Int)

  val starRegex = "position=<(.*),(.+)> velocity=<(.*),(.*)>".r

  val inputStars = Source.fromResource("input_day10.txt").getLines.toSeq
    .map { case starRegex(posX, posY, velX, velY) =>
        Star(posX.trim.toInt, posY.trim.toInt, velX.trim.toInt, velY.trim.toInt)
    }

  def max(by: Star => Int)(stars: Seq[Star]): Int = by(stars.maxBy(by))
  def min(by: Star => Int)(stars: Seq[Star]): Int = by(stars.minBy(by))

  @tailrec
  def minConstellation(stars: Seq[Star], prevHeight: Int = Int.MaxValue, second: Int = 0): (Seq[Star], Int) = {
    val nextStars = stars.map(s => Star(s.x + s.dx, s.y + s.dy, s.dx, s.dy))
    val height = max(_.y)(nextStars) - min(_.y)(nextStars)

    if (height > prevHeight) {
      (stars, second)
    } else {
      minConstellation(nextStars, height, second + 1)
    }
  }

  def starAtCoordinate(stars: Seq[Star])(coordinate: (Int, Int)): Boolean =
    stars.exists(s => s.x == coordinate._1 && s.y == coordinate._2)

  def asString(stars: Seq[Star]): String = {
    val rangeY = min(_.y)(stars) to max(_.y)(stars)
    val rangeX = min(_.x)(stars) to max(_.x)(stars)

    rangeY.map(y =>
      rangeX.map(x =>
        if (starAtCoordinate(stars)((x, y))) '#' else '.'
      ).mkString
    ).mkString("\n")
  }

  val constellationAndSeconds: (Seq[Star], Int) = minConstellation(inputStars)

  println(s"What message will eventually appear in the sky?\n\n${asString(constellationAndSeconds._1)}")
  println("\n")
  println(s"Exactly how many seconds would they have needed to wait for that message to appear? " +
    constellationAndSeconds._2)

}
