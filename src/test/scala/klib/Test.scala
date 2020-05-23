package klib

import klib.fp.order1.implicits._
import klib.handling._
import klib.fp.order1.instances._

object Test {

  trait Msg extends Message

  type ??[T] = MessageAccumulator[Msg, T]

  def main(args: Array[String]): Unit = {

    val i = 5.lift[??]

    println(i)

  }

}
