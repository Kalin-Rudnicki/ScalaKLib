package klib.fp

trait Monad[F[_]] extends Apply[F] {

  def flatten[A](self: F[F[A]]): F[A]

  def flatMap[A, B](self: F[A], f: A => F[B]): F[B] =
    flatten(map[A, F[B]](self, f))

  def flatApply[A, B](self: F[A], f: F[A => F[B]]): F[B] =
    flatten(apply[A, F[B]](self, f))

}
