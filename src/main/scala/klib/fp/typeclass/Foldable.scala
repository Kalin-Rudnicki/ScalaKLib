package klib.fp.typeclass

trait Foldable[F[_]] {

  def fold[A, B](self: F[A], _0: B)(join: (A, B) => B): B

}
