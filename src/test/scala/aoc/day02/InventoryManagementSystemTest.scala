package aoc.day02

import org.scalatest.{FunSuite, Matchers}
import InventoryManagementSystem._

class InventoryManagementSystemTest extends FunSuite with Matchers {

  // Part 1

  test("abcdef contains no letters that appear exactly two or three times.") {
    val lc = letterCount("abcdef")

    countsFor(2)(lc) should be (false)
    countsFor(3)(lc) should be (false)
  }
  test("bababc contains two a and three b, so it counts for both.") {
    val lc = letterCount("bababc")

    countsFor(2)(lc) should be (true)
    countsFor(3)(lc) should be (true)
  }
  test("abbcde contains two b, but no letter appears exactly three times.") {
    val lc = letterCount("abbcde")

    countsFor(2)(lc) should be (true)
    countsFor(3)(lc) should be (false)
  }
  test("abcccd contains three c, but no letter appears exactly two times.") {
    val lc = letterCount("abcccd")

    countsFor(2)(lc) should be (false)
    countsFor(3)(lc) should be (true)
  }
  test("aabcdd contains two a and two d, but it only counts once.") {
    val lc = letterCount("aabcdd")

    countsFor(2)(lc) should be (true)
    countsFor(3)(lc) should be (false)
  }
  test("abcdee contains two e.") {
    val lc = letterCount("abcdee")

    countsFor(2)(lc) should be (true)
    countsFor(3)(lc) should be (false)
  }
  test("ababab contains three a and three b, but it only counts once.") {
    val lc = letterCount("abcdef")

    countsFor(2)(lc) should be (false)
    countsFor(3)(lc) should be (false)
  }

  // Part 2

  test("the common letters between box ids should be fgij") {
    val input = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )

    commonLetters(input) should be ("fgij")
  }

}
