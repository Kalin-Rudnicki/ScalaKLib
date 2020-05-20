package klib.fp

object implicits {

  // =====| Conversions |=====

  implicit def toFunctorOps[F[_]: Functor, A](self: F[A]): FunctorOps[F, A] =
    new FunctorOps[F, A](self)

  implicit def toToApplyOps[A](self: A): ToApplyOps[A] =
    new ToApplyOps[A](self)

  implicit def toApplyOps[F[_]: Apply, A](self: F[A]): ApplyOps[F, A] =
    new ApplyOps[F, A](self)

  implicit def toApplyOpsF[F[_]: Apply, A, B](self: F[A => B]): ApplyOpsF[F, A, B] =
    new ApplyOpsF[F, A, B](self)

  implicit def toMonadOps[F[_]: Monad, A](self: F[A]): MonadOps[F, A] =
    new MonadOps[F, A](self)

  implicit def toFMonadOps[F[_]: Monad, A, B](self: F[A => B]): FMonadOps[F, A, B] =
    new FMonadOps[F, A, B](self)

  implicit def toMonadOpsF[F[_]: Monad, A, B](self: F[A => F[B]]): MonadOpsF[F, A, B] =
    new MonadOpsF[F, A, B](self)

  implicit def toMonadFlipOps_1_1[F[_], G[_], A](
      self: F[G[A]]
  )(implicit
      t: MonadFlipper_1_1[F, G]
  ): MonadFlipperOps_1_1[F, G, A] =
    new MonadFlipperOps_1_1[F, G, A](self)

  object auto {

    implicit def autoLift[F[_]: Apply, A](self: A): F[A] =
      self.lift[F]

  }

  // =====| Ops |=====

  class FunctorOps[F[_]: Functor, A](self: F[A]) {

    def map[B](f: A => B): F[B] =
      implicitly[Functor[F]].map(self, f)

    def #>[B](f: A => B): F[B] =
      map(f)

  }

  class ToApplyOps[A](self: A) {

    def lift[F[_]: Apply]: F[A] =
      implicitly[Apply[F]].lift(self)

    def ^[F[_]: Apply]: F[A] =
      lift

  }

  class ApplyOps[F[_]: Apply, A](self: F[A]) {

    def apply[B](f: F[A => B]): F[B] =
      implicitly[Apply[F]].apply(self, f)

    def *>[B](f: F[A => B]): F[B] =
      apply(f)

  }

  class ApplyOpsF[F[_]: Apply, A, B](self: F[A => B]) {

    def apply(a: F[A]): F[B] =
      implicitly[Apply[F]].apply(a, self)

    def <*(a: F[A]): F[B] =
      apply(a)

  }

  class MonadOps[F[_]: Monad, A](self: F[A]) {

    def flatMap[B](f: A => F[B]): F[B] =
      implicitly[Monad[F]].flatMap(self, f)

    def flatApply[B](f: F[A => F[B]]): F[B] =
      implicitly[Monad[F]].flatApply(self, f)

    def <##>[B](f: A => F[B]): F[B] =
      flatMap(f)

    def **>[B](f: F[A => F[B]]): F[B] =
      flatApply(f)

  }

  class FMonadOps[F[_]: Monad, A, B](self: F[A => B]) {

    def apply(a: F[F[A]]): F[B] = {
      val monad: Monad[F] = implicitly[Monad[F]]
      monad.apply(monad.flatten(a), self)
    }

    def <<*(a: F[F[A]]): F[B] =
      apply(a)

  }

  class MonadOpsF[F[_]: Monad, A, B](self: F[A => F[B]]) {

    def flatApply(a: F[A]): F[B] =
      implicitly[Monad[F]].flatApply(a, self)

    def <**(a: F[A]): F[B] =
      flatApply(a)

  }

  class MonadFlipperOps_1_1[F[_], G[_], A](self: F[G[A]])(implicit t: MonadFlipper_1_1[F, G]) {

    def flip: G[F[A]] =
      t.flip(self)

  }

}
