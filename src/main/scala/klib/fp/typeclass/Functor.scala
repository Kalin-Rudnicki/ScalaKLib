package klib.fp.typeclass

trait Functor[F[_]] {

  def forEach[A](self: F[A])(f: A => Unit): Unit

  def map[A, B](self: F[A])(f: A => B): F[B]

}
