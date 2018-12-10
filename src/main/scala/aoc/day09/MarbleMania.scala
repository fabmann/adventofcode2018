package aoc.day09

import cats.data.State

object MarbleMania extends App {

  val inputQtyPlayers: Int = 478
  val inputQtyMarbles: Int = 71240

  case class Elf(id: Int, score: Long)

  case class GameState(circle: Vector[Int],
                       remainingMarbles: Stream[Int],
                       elves: Seq[Elf],
                       curElf: Int)

  def initialState(qtyMarbles: Int, qtyPlayers: Int) = GameState(
    Vector(0),
    (1 to qtyMarbles).toStream,
    (1 to qtyPlayers).map(Elf(_, 0)),
    0
  )

  val nextMarble: State[GameState, Int] = State[GameState, Int](s =>
    (GameState(s.circle, s.remainingMarbles.drop(1), s.elves, s.curElf), s.remainingMarbles.head)
  )

  val nextElf: State[GameState, Elf] = State[GameState, Elf](s => {
    val elfId = (s.curElf % inputQtyPlayers) + 1
    (GameState(s.circle, s.remainingMarbles, s.elves, elfId), s.elves.find(_.id == elfId).get)
  })

  def rotateRight(is: Vector[Int], n: Int): Vector[Int] = {
    val length = is.length
    val shift = length - (n % length)
    is.drop(shift) ++ is.take(shift)
  }

  def rotateLeft(is: Vector[Int], n: Int): Vector[Int] = {
    val shift = n % is.length
    is.drop(shift) ++ is.take(shift)
  }

  def placeMarble(marble: Int): State[GameState, Unit] = State[GameState, Unit](s => {
    val nextCircle = marble +: rotateLeft(s.circle, 2)
    (GameState(nextCircle, s.remainingMarbles, s.elves, s.curElf), Unit)
  })

  val removeMarble: State[GameState, Int] = State[GameState, Int](s => {
    val rotatedCircle = rotateRight(s.circle, 7)
    (GameState(rotatedCircle.tail, s.remainingMarbles, s.elves, s.curElf), rotatedCircle.head)
  })

  def addToScore(marble: Int, elfId: Int): State[GameState, Unit] = State[GameState, Unit](s => {
    val elf = s.elves.find(_.id == elfId).get
    val nextElves = s.elves.filterNot(_.id == elfId) :+ Elf(elf.id, elf.score + marble)
    (GameState(s.circle, s.remainingMarbles, nextElves, s.curElf), Unit)
  })

  def elfScores(scoreMarble: Int, elf: Elf): State[GameState, Unit] =
    for {
      _ <- addToScore(scoreMarble, elf.id)
      secondScoreMarble <- removeMarble
      _ <- addToScore(secondScoreMarble, elf.id)
    } yield Unit

  val nextTurn: State[GameState, Unit] = {
    for {
      //_ <- State.get[GameState].map(s => println(s.curElf))
      elf <- nextElf
      marble <- nextMarble
      _ <- if (marble % 23 == 0) elfScores(marble, elf) else placeMarble(marble)
    } yield Unit
  }

  val gameLoop: State[GameState, Unit] = {
    State.get[GameState]
      .flatMap(s =>
        if (s.remainingMarbles.isEmpty) {
          State.pure(())
        } else {
          nextTurn.flatMap(_ => gameLoop)
        })
  }

  val winningElfScore = gameLoop.runS(initialState(inputQtyMarbles, inputQtyPlayers))
    .value.elves.maxBy(_.score).score

  println(s"What is the winning Elf's score? $winningElfScore")

  val winningElfScore100 = gameLoop.runS(initialState(inputQtyMarbles * 100, inputQtyPlayers))
    .value.elves.maxBy(_.score).score

  println(s"What would the new winning Elf's score be if the number of the last marble were 100 times larger? " +
    s"$winningElfScore100")

}
