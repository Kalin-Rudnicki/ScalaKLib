package klib

import klib.fp._
import klib.handling.{MessageAccumulator => MA, _}

object core extends ops with instances {

  implicit class AliveOps[T](value: T) {

    def alive: MA[Nothing, T] =
      MA.Alive(value)

    def alive[E <: Message](messages: E*): MA[E, T] =
      MA.Alive(value, messages: _*)

  }

}
