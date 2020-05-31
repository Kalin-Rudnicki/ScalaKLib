package klib

import org.scalactic.source.Position

import klib.handling._

object Test {

  trait Msg extends Message {

    override def toString: String =
      message

  }

  case class Err(message: String)(implicit val pos: Position) extends Msg

  type ??[T] = MessageAccumulator[Msg, T]

  class Parent {
    import scala.collection.mutable.{ListBuffer => MList}

    val children: MList[String] = MList()

    def addChild(name: String): Unit =
      children.append(name)

    def dump: Unit =
      println(children.toList)

  }

  def parent(f: Parent => Unit): Parent = {
    val p: Parent = new Parent
    f(p)
    p
  }

  def child(name: String)(implicit parent: Parent): Unit =
    parent.addChild(name)

  def main(args: Array[String]): Unit = {}
}
