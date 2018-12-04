package aoc.day03

import scala.io.Source

object NoMatterHowYouSliceIt extends App {

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int)

  val claimRegex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  val inputClaims = Source.fromResource("input_day03.txt")
    .getLines
    .map { case claimRegex(id, x, y, width, height) =>
      Claim(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
    }
    .toSeq

  val coords: Seq[(Int, Int)] =
    for {
      x <- 0 until 1000
      y <- 0 until 1000
    } yield (x, y)

  val coordsWithClaims: Map[(Int, Int), Seq[Claim]] = coords.map(co =>
    (co, inputClaims.filter(cl => Function.tupled(isWithinClaim(cl)_)(co)))
  ).toMap

  val inchesWithinTwoOrMoreClaims =
    coordsWithClaims.count { case (_, cls) => cls.length >= 2 }

  println(s"Amount of square inches within two or more claims: $inchesWithinTwoOrMoreClaims")

  val onlyNonOverlappingClaim = inputClaims.filter(cl =>
    !claimCoordinates(cl).exists(co => coordsWithClaims(co).length != 1)
  ).head

  println(s"The claim ID of a claim without overlapping claims: ${onlyNonOverlappingClaim.id}")

  def isWithinClaim(claim: Claim)(x: Int, y: Int): Boolean = {
      x >= claim.x && x < (claim.x + claim.width) &&
      y >= claim.y && y < (claim.y + claim.height)
  }

  def claimCoordinates(claim: Claim): Seq[(Int, Int)] = for {
    x <- claim.x until claim.x + claim.width
    y <- claim.y until claim.y + claim.height
  } yield (x,y)

}
