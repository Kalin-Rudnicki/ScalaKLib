package klib.fp.typeclass

object Inverter {

  // TODO (KR) : Something in here can be inferred from Applicative

  trait External[F[_]]

  trait Internal[G[_]]

  trait Wrapped[A] {

    def _0: A

  }

}
