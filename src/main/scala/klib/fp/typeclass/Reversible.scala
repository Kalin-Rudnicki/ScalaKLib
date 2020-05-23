package klib.fp.typeclass

trait Reversible[F[_]] {

  def reverse[A](next: F[A], prev: F[A]): F[A]

}
