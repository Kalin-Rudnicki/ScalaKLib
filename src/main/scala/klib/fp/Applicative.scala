package klib.fp

trait Applicative[F[_]] extends Functor[F] {

  def lift[A](self: A): F[A]

  def apply[A, B](f: F[A => B], self: F[A]): F[B]

}
