package aoc.day08

import scala.annotation.tailrec
import scala.io.Source

object MemoryManeuver extends App {

  case class Node(childNodes: Seq[Node], metadata: Seq[Int])

  val input: Seq[Int] = Source.fromResource("input_day08.txt").mkString.split(" ").map(_.toInt).toList

  def readNode(ints: Seq[Int]): (Node, Seq[Int]) = {
    ints match {
      case qtyChildren :: qtyMetadata :: tail =>
        val (children, rest) = readChildren(qtyChildren, tail)
        val (metadata, rest1) = readMetadata(qtyMetadata, rest)
        (Node(children, metadata), rest1)
    }
  }

  @tailrec
  def readChildren(n: Int, tail: Seq[Int], acc: Seq[Node] = Seq.empty): (Seq[Node], Seq[Int]) = {
    if (n == 0) {
      (acc, tail)
    } else {
      val (child, rest) = readNode(tail)
      readChildren(n - 1, rest, acc :+ child)
    }
  }

  def readMetadata(n: Int, rest: Seq[Int]): (Seq[Int], Seq[Int]) = {
    (rest.take(n), rest.drop(n))
  }

  def sumMetadata(node: Node): Int =
    node.childNodes.map(sumMetadata).sum + node.metadata.sum

  val rootNode: Node = readNode(input)._1

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
