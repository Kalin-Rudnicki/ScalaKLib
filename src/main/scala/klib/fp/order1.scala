package klib.fp

object order1 extends order1 {

  object ToOps1 extends ToOps1

  object Auto1 extends Auto1

  trait ToOps1 {

    import Ops1._

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

  }

  trait Auto1 {

    implicit def autoLift[F[_]: Apply, A](a: A): F[A] =
      implicitly[Apply[F]].lift(a)

  }

}

trait order1 {

  trait Functor[F[_]] {

    def map[A, B](f: A => B, self: F[A]): F[B]

  }

  trait Apply[F[_]] extends Functor[F] {

    def lift[A](self: A): F[A]

    def apply[A, B](f: F[A => B], self: F[A]): F[B]

  }

  trait Monad[F[_]] extends Apply[F] {

    def flatten[A](self: F[F[A]]): F[A]

    def flatMap[A, B](f: A => F[B], self: F[A]): F[B] =
      flatten(map(f, self))

    def flatApply[A, B](f: F[A => F[B]], self: F[A]): F[B] =
      flatten(apply(f, self))

  }

  object Ops1 {

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

}
