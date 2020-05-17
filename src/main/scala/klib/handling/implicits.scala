package klib.handling

import klib.handling.MessageAccumulator._

object implicits {

  implicit def aliveFromValue[T](v: T): MessageAccumulator[Nothing, T] =
    Alive(v)

  implicit def deadFromMessage[M <: Message](m: M): MessageAccumulator[M, Nothing] =
    Dead(m)

}
