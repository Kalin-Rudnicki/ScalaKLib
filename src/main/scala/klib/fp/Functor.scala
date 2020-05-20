package klib.fp

trait Functor[F[_]] {

  def map[A, B](self: F[A], f: A => B): F[B]

}
