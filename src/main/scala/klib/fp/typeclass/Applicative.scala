package klib.fp.typeclass

trait Applicative[F[_]] extends Functor[F] {

  def lift[A](self: A): F[A]

  def apply[A, B](self: F[A])(f: F[A => B]): F[B]

}
