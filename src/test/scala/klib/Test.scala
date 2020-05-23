package klib

import klib.handling._
import klib.handling.MessageAccumulator._
import klib.fp.ops._
import klib.fp.instances._
import klib.fp.conversions._

object Test {

  trait Msg extends Message

  type ??[T] = MessageAccumulator[Msg, T]

  def main(args: Array[String]): Unit = {

    val i: List[??[Int]] = List[??[Int]](1.alive, 2.alive, 3.alive, 4.alive)

    val j: ??[List[Int]] = i
      .foldO[??[List[Int]]](Nil.alive) { (int, listInt) =>
        listInt.flatMap { _listInt =>
          int.flatMap { _int =>
            (_int :: _listInt).alive
          }
        }
      }
      .map(_.reverse)

    println(i)
    println(j)

  }

}
