package klib

import klib.fp._
import klib.handling.{MessageAccumulator => MA, _}

object core {
  
  export ops._
  export ops.{given _}
  export instances._
  export instances.{given _}
  
}

/*
implicit class AliveOps[T](value: T) {
  
  def alive: MA[Nothing, T] =
    MA.Alive(value)
  
  def alive[E <: Message](messages: E*): MA[E, T] =
    MA.Alive(value, messages: _*)
  
}
 */