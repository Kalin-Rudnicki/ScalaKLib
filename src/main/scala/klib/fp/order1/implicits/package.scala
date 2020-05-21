package klib.fp.order1

import klib.fp.order1.types._
import klib.fp.order1.helpers._

package object implicits {

  implicit def toFunctorOps[F[_]: Functor, A](self: F[A]): FunctorOps[F, A] =
    new FunctorOps(self)

  implicit def toToApplyOps[A](self: A): ToApplyOps[A] =
    new ToApplyOps(self)

  implicit def toApplyOps[F[_]: Apply, A, B](f: F[A => B]): ApplyOps[F, A, B] =
    new ApplyOps(f)

  implicit def toApplyOpsFlipped[F[_]: Apply, A](self: F[A]): ApplyOpsFlipped[F, A] =
    new ApplyOpsFlipped(self)

  implicit def toMonadOps_FlatApplyFlipped[F[_]: Monad, A](self: F[A]): MonadOps_ApplyFlipped[F, A] =
    new MonadOps_ApplyFlipped(self)

  implicit def toFlatApplyOps[F[_]: Monad, A, B](f: F[A => F[B]]): FlatApplyOps[F, A, B] =
    new FlatApplyOps(f)

  object auto {

    implicit def autoLift[F[_]: Apply, A](a: A): F[A] =
      implicitly[Apply[F]].lift(a)

  }

}
