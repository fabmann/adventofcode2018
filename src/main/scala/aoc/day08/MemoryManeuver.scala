package aoc.day08

import cats.data.State

import scala.io.Source

object MemoryManeuver extends App {

  case class Node(childNodes: Seq[Node], metadata: Seq[Int])

  case class Header(qtyChildren: Int, qtyMetadata: Int)

  val input: Seq[Int] = Source.fromResource("input_day08.txt").mkString.split(" ").map(_.toInt).toList

  val readHeader: State[Seq[Int], Header] = State[Seq[Int], Header] {
    case qtyChildren :: qtyMetadata :: tail => (tail, Header(qtyChildren, qtyMetadata))
  }

  def readChildren(n: Int, acc: Seq[Node] = Seq.empty): State[Seq[Int], Seq[Node]] =
    if (n == 0) {
      State[Seq[Int], Seq[Node]](tail => (tail, acc))
    } else {
      readNode.flatMap(child => readChildren(n - 1, acc :+ child))
    }

  def readMetadata(n: Int): State[Seq[Int], Seq[Int]] = State[Seq[Int], Seq[Int]](rest =>
    (rest.drop(n), rest.take(n))
  )

  val readNode: State[Seq[Int], Node] =
    for {
      header   <- readHeader
      children <- readChildren(header.qtyChildren)
      metadata <- readMetadata(header.qtyMetadata)
    } yield Node(children, metadata)

  def sumMetadata(node: Node): Int =
    node.childNodes.map(sumMetadata).sum + node.metadata.sum

  val rootNode: Node = readNode.runA(input).value

  println(s"What is the sum of all metadata entries? ${sumMetadata(rootNode)}")

  def value(node: Node): Int = node match {
    case Node(Nil, metadata) => metadata.sum
    case Node(childNodes, metadata) =>
      metadata.flatMap(i => childNodes.lift(i - 1))
        .map(value)
        .sum
  }

  println(s"What is the value of the root node? ${value(rootNode)}")

}
