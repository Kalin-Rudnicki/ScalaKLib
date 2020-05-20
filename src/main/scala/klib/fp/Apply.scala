package klib.fp

trait Apply[F[_]] extends Functor[F] {

  def apply[A, B](self: F[A], f: F[A => B]): F[B]

  def lift[A](self: A): F[A]

}
