package klib.fp.typeclass

trait ZeroAdd[F[_]] {

  def _0[A]: F[A]

  def add[A](self: F[A], that: A): F[A]

}
