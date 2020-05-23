package klib.fp

object Ops {

  implicit class FunctorOps[F[_]: Functor, A](self: F[A]) {

    def forEach(f: A => Unit): Unit =
      implicitly[Functor[F]].forEach(f, self)

    def map[B](f: A => B): F[B] =
      implicitly[Functor[F]].map(f, self)

    def >#>[B](f: A => B): F[B] =
      map(f)

  }

  implicit class ToApplyOps[A](self: A) {

    def lift[F[_]: Applicative]: F[A] =
      implicitly[Applicative[F]].lift(self)

    def ^[F[_]: Applicative]: F[A] =
      lift

  }

  implicit class ApplyOps[F[_]: Applicative, A, B](f: F[A => B]) {

    def apply(self: F[A]): F[B] =
      implicitly[Applicative[F]].apply(f, self)

    def <*<(self: F[A]): F[B] =
      apply(self)

  }

  implicit class ApplyOpsFlipped[F[_]: Applicative, A](self: F[A]) {

    def applyTo[B](f: F[A => B]): F[B] =
      implicitly[Applicative[F]].apply(f, self)

    def >*>[B](f: F[A => B]): F[B] =
      applyTo(f)

  }

  implicit class MonadOps_ApplyFlipped[F[_]: Monad, A](self: F[A]) {

    def flatMap[B](f: A => F[B]): F[B] =
      implicitly[Monad[F]].flatMap(f, self)

    def flatApplyTo[B](f: F[A => F[B]]): F[B] =
      implicitly[Monad[F]].flatApply(f, self)

    def >##>[B](f: A => F[B]): F[B] =
      flatMap(f)

    def >**>[B](f: F[A => F[B]]): F[B] =
      flatApplyTo(f)

  }

  implicit class FlatApplyOps[F[_]: Monad, A, B](f: F[A => F[B]]) {

    def flatApply(self: F[A]): F[B] =
      implicitly[Monad[F]].flatApply(f, self)

    def <**<(self: F[A]): F[B] =
      flatApply(self)

  }

}
