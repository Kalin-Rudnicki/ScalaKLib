package klib.fp.typeclass

trait FoldableOnce[F[_]] {
  
  def fold[A, B](self: F[A])(present: A => B)(missing: => B): B
  
}
