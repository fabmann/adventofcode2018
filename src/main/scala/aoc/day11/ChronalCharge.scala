package aoc.day11

object ChronalCharge extends App {

  val gridSerialNumber = 3214
  val range = 1 to 300

  type Square = Seq[FuelCell]

  case class FuelCell(x: Int, y: Int) {

    def powerLevel(serialNumber: Int): Int = {
      val rackId = this.x + 10
      (((rackId * this.y + serialNumber) * rackId) / 100) % 10 - 5
    }

    def square(size: Int): Square = {
      for {
        x <- this.x until this.x + size
        y <- this.y until this.y + size
      } yield FuelCell(x, y)
    }

  }

  def getPowerLevelField(serialNumber: Int): Vector[Vector[Int]] = {
    range.map(x =>
      range.map( y =>
        FuelCell(x,y).powerLevel(serialNumber)
      ).toVector
    ).toVector
  }

  def getSquares(size: Int): Seq[Square] = {
    val rangeTopLeft = range.reverse.drop(size - 1).reverse
    for {
      x <- rangeTopLeft
      y <- rangeTopLeft
    } yield FuelCell(x, y).square(size)
  }

  def getTotalPower(field: Vector[Vector[Int]])(square: Square): Int = {
    square.map { case FuelCell(x,y) => field(x - 1)(y - 1) }.sum
  }

  val powerLevelField: Vector[Vector[Int]] = getPowerLevelField(gridSerialNumber)
  val totalPowerSquare: Square => Int = getTotalPower(powerLevelField)

  def topLeftCellWithLargestTotalPowerSquare(serialNumber: Int)(squareSize: Int): FuelCell = {
    getSquares(squareSize).maxBy(totalPowerSquare).head
  }

  println("What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power? " +
    topLeftCellWithLargestTotalPowerSquare(gridSerialNumber)(3))

  /**
    * Creates all power fields up to a certain square size. A power field is a matrix, where each coordinate
    * corresponds to the total power of a square, for which the coordinate would be the top left corner. The result
    * contains the power fields for all square sizes.
    *
    * E.g. the total power of a square of size 5x5 at coordinate (23,42) can be addressed as
    * <pre>
    * val result = getPowerFields(5)
    * result(5-1)(23-1)(42-1)
    * </pre>
    * (the indices of the vectors start with 0, while in the puzzle they start with 1)
    *
    * @param size the maximum square size
    * @return all power fields for squares up to size x size
    */
  def getPowerFields(size: Int): Vector[Vector[Vector[Int]]] = {
    if (size == 1) {
      Vector(powerLevelField)
    } else {
      val lowerFields = getPowerFields(size - 1)

      lowerFields :+ getPowerFieldFromLower(size, lowerFields.last)
    }
  }

  /**
    * Creates a field where each x,y coordinate holds the total power of the square of a size, for which the
    * coordinate would be the top left corner. The value is calculated by reusing the result for the power field of
    * smaller squares and just adding the power levels of the missing outer coordinates of the new square.
    *
    * @param size   the size of the squares
    * @param lower  the power field for size-1 sized squares
    * @return the power field for squares of size
    */
  def getPowerFieldFromLower(size: Int, lower: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    (1 until lower.length).map(x =>
      (1 until lower(x).length).map(y =>
        lower(x - 1)(y - 1) + missingOuterSquareCoords(x - 1, y - 1, size - 1)
          .map { case (a, b) => powerLevelField(a)(b) }.sum
      ).toVector
    ).toVector
  }

  def missingOuterSquareCoords(x: Int, y: Int, s: Int): Seq[(Int, Int)] ={
    (x to x+s).map(i => (i, y+s)) ++ (y until y+s).map(i => (x+s, i))
  }

  val powerFields: Vector[Vector[Vector[Int]]] = getPowerFields(300)

  val largestTotalPowerSquare = (for {
    size <- 1 to 300
    x <- 1 to 300-(size-1)
    y <- 1 to 300-(size-1)
  } yield (size, x, y, powerFields(size-1)(x-1)(y-1))).maxBy(_._4)

  println("What is the X,Y,size identifier of the square with the largest total power?\n"+
    s"${largestTotalPowerSquare._2},${largestTotalPowerSquare._3},${largestTotalPowerSquare._1}")

}
