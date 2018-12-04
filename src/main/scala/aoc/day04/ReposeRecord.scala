package aoc.day04

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.io.Source

object ReposeRecord extends App {

  case class Record(timestamp: LocalDateTime, payload: Payload)

  sealed trait Payload
  case class BeginsShift(guardId: Int) extends Payload
  case object FallsAsleep extends Payload
  case object WakesUp extends Payload

  val recordRegex = "\\[(.*)\\] (.*)".r
  val beginShiftRegex = "Guard #(\\d+) begins shift".r

  val input: Seq[Record] = Source.fromResource("input_day04.txt")
    .getLines
    .map { case recordRegex(timestampString, payloadString) =>
      val timestamp = LocalDateTime.parse(timestampString, DateTimeFormatter.ofPattern("y-M-d H:m"))
      val payload = payloadString match {
        case beginShiftRegex(idString) => BeginsShift(idString.toInt)
        case "falls asleep" => FallsAsleep
        case "wakes up" => WakesUp
      }
      Record(timestamp, payload)
    }
    .toSeq
    .sortBy(_.timestamp)(_ compareTo _)

  val guardsToMinutesAsleep: Map[Int, Seq[Range]] =
    input.foldLeft(
      (Map.empty: Map[Int, Seq[Range]], None: Option[Int], None: Option[Int])
    ) {
      case ((result, curGuard, fallAsleepMinute), record) =>
        record.payload match {
          case BeginsShift(guardId) => (result, Some(guardId), None)
          case FallsAsleep => (result, curGuard, Some(record.timestamp.getMinute))
          case WakesUp =>
            val rangeAdded = addRange(result, curGuard.get, fallAsleepMinute.get until record.timestamp.getMinute)
            (rangeAdded, curGuard, None)
        }
    }._1

  def addRange(guardsToMinuteRanges: Map[Int, Seq[Range]], guard: Int, range: Range): Map[Int, Seq[Range]] =
    guardsToMinuteRanges + (guard -> (guardsToMinuteRanges.getOrElse(guard, Seq.empty[Range]) :+ range))

  def getEntryWithHighestValue(map: Map[Int, Int]): (Int, Int) =
    map.toSeq.sortBy(_._2)(Ordering.Int.reverse).head

  def minuteMostSleptByGuard(guardId: Int): Int = {
    val minuteToOccurrenceCount = getMinuteToOccurrenceCount(guardsToMinutesAsleep(guardId))
    getEntryWithHighestValue(minuteToOccurrenceCount)._1
  }

  val guardWithMostMinutesAsleep: Int =
    getEntryWithHighestValue(guardsToMinutesAsleep.mapValues(rs => rs.map(_.sum).sum))._1

  println(s"The ID of the guard who slept most multiplied by the minute most slept by him " +
    s"is: ${guardWithMostMinutesAsleep * minuteMostSleptByGuard(guardWithMostMinutesAsleep)}")

  val guardWithMostSleptMinute: Int = {
    val guardsToHighestOccurrenceCount = guardsToMinutesAsleep.mapValues(ranges => getHighestOccurrenceCount(ranges))
    getEntryWithHighestValue(guardsToHighestOccurrenceCount)._1
  }

  def getHighestOccurrenceCount(ranges: Seq[Range]): Int =
    getEntryWithHighestValue(getMinuteToOccurrenceCount(ranges))._2

  def getMinuteToOccurrenceCount(minuteRanges: Seq[Range]): Map[Int, Int] =
    minuteRanges
      .flatMap(_.toSeq)
      .groupBy(i => i)
      .mapValues(_.length)

  println(s"The ID of the guard with the most reoccurring minute asleep multiplied by the minute " +
    s"is: ${guardWithMostSleptMinute * minuteMostSleptByGuard(guardWithMostSleptMinute)}")

}
