package klib.fp.order1

import klib.fp.order1.types._

package object helpers {

  class FunctorOps[F[_]: Functor, A](self: F[A]) {

    def map[B](f: A => B): F[B] =
      implicitly[Functor[F]].map(f, self)

    def >#>[B](f: A => B): F[B] =
      map(f)

  }

  class ToApplyOps[A](self: A) {

    def lift[F[_]: Apply]: F[A] =
      implicitly[Apply[F]].lift(self)

    def ^[F[_]: Apply]: F[A] =
      lift

  }

  class ApplyOps[F[_]: Apply, A, B](f: F[A => B]) {

    def apply(self: F[A]): F[B] =
      implicitly[Apply[F]].apply(f, self)

    def <*<(self: F[A]): F[B] =
      apply(self)

  }

  class ApplyOpsFlipped[F[_]: Apply, A](self: F[A]) {

    def applyTo[B](f: F[A => B]): F[B] =
      implicitly[Apply[F]].apply(f, self)

    def >*>[B](f: F[A => B]): F[B] =
      applyTo(f)

  }

  class MonadOps_ApplyFlipped[F[_]: Monad, A](self: F[A]) {

    def flatMap[B](f: A => F[B]): F[B] =
      implicitly[Monad[F]].flatMap(f, self)

    def flatApplyTo[B](f: F[A => F[B]]): F[B] =
      implicitly[Monad[F]].flatApply(f, self)

    def >##>[B](f: A => F[B]): F[B] =
      flatMap(f)

    def >**>[B](f: F[A => F[B]]): F[B] =
      flatApplyTo(f)

  }

  class FlatApplyOps[F[_]: Monad, A, B](f: F[A => F[B]]) {

    def flatApply(self: F[A]): F[B] =
      implicitly[Monad[F]].flatApply(f, self)

    def <**<(self: F[A]): F[B] =
      flatApply(self)

  }

}
