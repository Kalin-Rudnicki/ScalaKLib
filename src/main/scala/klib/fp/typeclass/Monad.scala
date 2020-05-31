package klib.fp.typeclass

trait Monad[F[_]] extends Applicative[F] {

  def flatten[A](self: F[F[A]]): F[A]

  def flatMap[A, B](self: F[A])(f: A => F[B]): F[B] =
    flatten(map(self)(f))

  def flatApply[A, B](self: F[A])(f: F[A => F[B]]): F[B] =
    flatten(apply(self)(f))

}
