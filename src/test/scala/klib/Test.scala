package klib

import org.scalactic.source.Position

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import klib.handling._

object Test {

  trait Msg extends Message {

    override def toString: String =
      message

  }

  case class Ex(message: String)(implicit val pos: Position) extends Msg

  type ??[T] = MessageAccumulator[Msg, T]

  def main(args: Array[String]): Unit = {

    val res0: List[Option[Int]] =
      List(
        1._lift[Option],
        2._lift[Option],
        3._lift[Option]
      )
    val res1: List[Option[Int]] =
      List(
        1._lift[Option],
        2._lift[Option],
        None
      )

    println(res0)
    println(res0.invert)
    println(res0.invertR)
    println

    println(res1)
    println(res1.invert)
    println(res1.invertR)

    val res2: List[??[Int]] =
      List(
        1._lift[??],
        2._lift[??],
        3._lift[??]
      )

    val res3: List[??[Int]] =
      List(
        Alive(1, Ex("'1'")),
        Alive(2, Ex("'2'")),
        Alive(3, Ex("'3'"))
      )

    val res4: List[??[Int]] =
      List(
        Alive(1, Ex("'1'")),
        Alive(2, Ex("'2'")),
        Dead(Ex("'3'"))
      )

    println(res2)
    println(res2.invert)
    println(res2.invertR)
    println

    println(res3)
    println(res3.invert)
    println(res3.invertR)
    println

    println(res4)
    println(res4.invert)
    println(res4.invertR)
    println

  }

}
