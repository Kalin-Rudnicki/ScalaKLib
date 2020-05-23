package klib.fp

trait Monad[F[_]] extends Applicative[F] {

  def flatten[A](self: F[F[A]]): F[A]

  def flatMap[A, B](f: A => F[B], self: F[A]): F[B] =
    flatten(map(f, self))

  def flatApply[A, B](f: F[A => F[B]], self: F[A]): F[B] =
    flatten(apply(f, self))

}
