package klib.handling

import scala.language.implicitConversions

import org.scalactic.source.Position

import klib.handling.MessageAccumulator._

object Implicits {

  implicit def aliveFromValue[T](v: T): MessageAccumulator[Nothing, T] =
    Alive(v)

  implicit def deadFromMessage[M <: Message](m: M)(implicit pos: Position): MessageAccumulator[M, Nothing] =
    Dead(m)

  implicit class ReWrapper[E <: Message, T, W[_]](ma: W[MessageAccumulator[E, T]]) {

    def reWrap(implicit f: T => W[T]): W[MessageAccumulator[E, T]] = ???

  }

}
