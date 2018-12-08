package aoc.day07

import scala.annotation.tailrec
import scala.io.Source

object TheSumOfItsParts extends App {

  case class Before(stepBefore: Char, stepAfter: Char)

  val beforeRegex = "Step (.) must be finished before step (.) can begin.".r

  val beforeRelations = Source.fromResource("input_day07.txt").getLines
    .map { case beforeRegex(a, b) => Before(a.head, b.head) }
    .toSet

  val alphabeticallySortedSteps: Seq[Char] = beforeRelations.toSeq
    .flatMap(br => Seq(br.stepBefore, br.stepAfter))
    .distinct
    .sorted

  def canBeDone(step: Char, done: String): Boolean =
    !beforeRelations
      .filter(s => !done.contains(s.stepBefore))
      .exists(_.stepAfter == step)

  @tailrec
  def completionOrder(result: String, remainingSteps: Seq[Char], relations: Set[Before]): String = {
    if (remainingSteps.isEmpty) {
      result
    } else {
      val nextStep = remainingSteps.find(step => canBeDone(step, result)).get
      completionOrder(
        result + nextStep,
        remainingSteps.filterNot(_ == nextStep),
        relations)
    }
  }

  val orderedSteps: String = completionOrder("", alphabeticallySortedSteps, beforeRelations)

  println(s"In what order should the steps in your instructions be completed? $orderedSteps")

  def duration(step: Char): Int = {
    step.toInt - 'A'.toInt + 1 + 60
  }

  val numWorkers = 5

  sealed trait Worker {
    def id: Int
  }
  case class IdleWorker(id: Int) extends Worker
  case class BusyWorker(id: Int, step: Char) extends Worker

  case class Event(second: Int, stepFinished: Option[Char])

  case class Progress(workers: Seq[Worker],
                      remainingSteps: Seq[Char],
                      done: String) {

    def finishStep(step: Char): Progress = {
      println(s"  finished $step")
      Progress(setWorkerIdle(step), this.remainingSteps, this.done + step)
    }

    def startWork(step: Char): Progress = {
      Progress(setWorkerBusy(step), remainingSteps.filter(_ != step), this.done)
    }

    private def setWorkerIdle(stepWorkedOn: Char): Seq[Worker] = {
      val workerId = workers.find {
        case BusyWorker(_, workingOn) => workingOn == stepWorkedOn
        case _ => false
      }.get.id

      workers.filterNot(_.id == workerId) :+ IdleWorker(workerId)
    }

    private def setWorkerBusy(step: Char): Seq[Worker] = {
      val workerId = workers.find(_.isInstanceOf[IdleWorker]).get.id

      workers.filterNot(_.id == workerId) :+ BusyWorker(workerId, step)
    }

  }

  val initialProgress = Progress(
    (1 to 5).map(IdleWorker),
    orderedSteps,
    ""
  )

  @tailrec
  def assignWork(curSecond: Int, progress: Progress, accEvents: Seq[Event]): (Progress, Seq[Event]) = {
    val nextSteps = progress.remainingSteps.filter(canBeDone(_, progress.done)).sorted
    val idleWorkers = progress.workers.filter(_.isInstanceOf[IdleWorker])
    if (nextSteps.isEmpty || idleWorkers.isEmpty) {
      (progress, accEvents)
    } else {
      val newEvent = Event(curSecond + duration(nextSteps.head), Some(nextSteps.head))
      val newProgress = progress.startWork(nextSteps.head)
      println(s"  start work on ${nextSteps.head}")
      assignWork(curSecond, newProgress, accEvents :+ newEvent)
    }
  }

  @tailrec
  def run(progress: Progress, remainingEvents: Seq[Event]): Int = {
    val sortedEvents = remainingEvents.sortBy(_.second)
    sortedEvents match {
      case Seq(lastEvent) if progress.remainingSteps.isEmpty => lastEvent.second
      case Event(second, stepOption) :: eventTail =>
        println(s"second $second, workers: ${progress.workers.sortBy(_.id)}")
        val progress1 = stepOption
          .map(progress.finishStep)
          .getOrElse(progress)

        val (progress2, newEvents) = assignWork(second, progress1, Seq.empty)

        run(progress2, eventTail ++ newEvents)
    }

  }

  val timeToCompletion = run(initialProgress, Seq(Event(0, None)))

  println(s"How long will it take to complete all of the steps? $timeToCompletion")

}
