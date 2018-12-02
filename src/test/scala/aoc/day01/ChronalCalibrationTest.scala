package aoc.day01

import org.scalatest.FunSuite
import ChronalCalibration._
import org.scalatest.Matchers._

class ChronalCalibrationTest extends FunSuite {

  // Part 1

  test("frequency changes of +1, -2, +3, +1 should result in 3") {
    val input = List(+1, -2, +3, +1)

    resultingFrequency(0, input) should be (3)
  }

  test("frequency changes of +1, +1, +1 should result in 3") {
    val input = List(+1, +1, +1)

    resultingFrequency(0, input) should be (3)
  }

  test("frequency changes of +1, +1, -2 should result in 0") {
    val input = List(+1, +1, -2)

    resultingFrequency(0, input) should be (0)
  }

  test("frequency changes of -1, -2, -3 should result in -6") {
    val input = List(-1, -2, -3)

    resultingFrequency(0, input) should be (-6)
  }

  // Part 2

  test("with frequency changes of +1, -2, +3, +1 the first reached twice should be 2") {
    val input = List(+1, -2, +3, +1)

    firstFrequencyReachedTwice(0, input) should be (2)
  }

  test("with frequency changes of +1, -1 the first reached twice should be 0") {
    val input = List(+1, -1)

    firstFrequencyReachedTwice(0, input) should be (0)
  }

  test("with frequency changes of +3, +3, +4, -2, -4 the first reached twice should be 10") {
    val input = List(+3, +3, +4, -2, -4)

    firstFrequencyReachedTwice(0, input) should be (10)
  }

  test("with frequency changes of -6, +3, +8, +5, -6 the first reached twice should be 5") {
    val input = List(-6, +3, +8, +5, -6)

    firstFrequencyReachedTwice(0, input) should be (5)
  }

  test("with frequency changes of +7, +7, -2, -7, -4 the first reached twice should be 14") {
    val input = List(+7, +7, -2, -7, -4)

    firstFrequencyReachedTwice(0, input) should be (14)
  }

}
