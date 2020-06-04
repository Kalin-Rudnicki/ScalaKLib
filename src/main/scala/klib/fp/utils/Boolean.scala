package klib.fp.utils

implicit class BooleanOps(self: Boolean) {
  
  def ?[A] (onTrue: => A): Ternary[A] =
    new Ternary(self, onTrue)
  
  def option[A](wrap: => A): Option[A] =
    self ? Some(wrap) | None
  
}

class Ternary[A] private[utils] (bool: => Boolean, onTrue: => A) {
  
  def | (onFalse: => A): A =
    if (bool)
      onTrue
    else
      onFalse
  
}
