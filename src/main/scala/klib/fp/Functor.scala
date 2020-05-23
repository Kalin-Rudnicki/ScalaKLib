package klib.fp

trait Functor[F[_]] {

  def forEach[A](f: A => Unit, self: F[A]): Unit

  def map[A, B](f: A => B, self: F[A]): F[B]

}
