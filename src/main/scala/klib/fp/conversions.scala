package klib.fp

import klib.fp.typeclass.Applicative

object conversions {

  implicit def autoLift[F[_]: Applicative, A](self: A): F[A] =
    implicitly[Applicative[F]].lift(self)

}
