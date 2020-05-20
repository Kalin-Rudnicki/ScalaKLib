package klib.fp

object order2 extends order2 {

  object ToOps2 extends ToOps2

  object Auto2 extends Auto2

  trait ToOps2 {

    import Ops2._

    implicit def toBiFunctorOps[F[_, _]: BiFunctor, E, A](self: F[E, A]): BiFunctorOps[F, E, A] =
      new BiFunctorOps(self)

    implicit def toBiToApplyOps[A](self: A): ToBiApplyOps[A] =
      new ToBiApplyOps(self)

    implicit def toBiApplyOps[F[_, _]: BiApply, E, A, B](f: F[E, A => B]): BiApplyOps[F, E, A, B] =
      new BiApplyOps(f)

    implicit def toBiApplyOpsFlipped[F[_, _]: BiApply, E, A](self: F[E, A]): BiApplyOpsFlipped[F, E, A] =
      new BiApplyOpsFlipped(self)

    implicit def toBiMonadOps_FlatApplyFlipped[F[_, _]: BiMonad, E, A](self: F[E, A]): BiMonadOps_ApplyFlipped[F, E, A] =
      new BiMonadOps_ApplyFlipped(self)

    implicit def toBiFlatApplyOps[F[_, _]: BiMonad, E, A, B](f: F[E, A => F[E, B]]): BiFlatApplyOps[F, E, A, B] =
      new BiFlatApplyOps(f)

  }

  trait Auto2 {

    implicit def biAutoLift[F[_, _]: BiApply, E, A](a: A): F[E, A] =
      implicitly[BiApply[F]].lift(a)

  }

}

trait order2 {

  // TODO (KR) : It might be better to use 'E' ?
  //           : We will see how well it figure out the types

  trait BiFunctor[F[_, _]] {

    def map[E, A, B](f: A => B, self: F[E, A]): F[E, B]

  }

  trait BiApply[F[_, _]] extends BiFunctor[F] {

    def lift[E, A](self: A): F[E, A]

    def apply[E, A, B](f: F[E, A => B], self: F[E, A]): F[E, B]

  }

  trait BiMonad[F[_, _]] extends BiApply[F] {

    def flatten[E, A](self: F[E, F[E, A]]): F[E, A]

    def flatMap[E, A, B](f: A => F[E, B], self: F[E, A]): F[E, B] =
      flatten(map(f, self))

    def flatApply[E, A, B](f: F[E, A => F[E, B]], self: F[E, A]): F[E, B] =
      flatten(apply(f, self))

  }

  object Ops2 {

    class BiFunctorOps[F[_, _]: BiFunctor, E, A](self: F[E, A]) {

      def map[B](f: A => B): F[E, B] =
        implicitly[BiFunctor[F]].map(f, self)

      def >#>[B](f: A => B): F[E, B] =
        map(f)

    }

    class ToBiApplyOps[A](self: A) {

      def lift2[F[_, _]: BiApply, E]: F[E, A] =
        implicitly[BiApply[F]].lift(self)

      def ^[F[_, _]: BiApply, E]: F[E, A] =
        lift2

    }

    class BiApplyOps[F[_, _]: BiApply, E, A, B](f: F[E, A => B]) {

      def apply(self: F[_, A]): F[E, B] =
        implicitly[BiApply[F]].apply(f, self)

      def <*<(self: F[_, A]): F[E, B] =
        apply(self)

    }

    class BiApplyOpsFlipped[F[_, _]: BiApply, E, A](self: F[E, A]) {

      def applyTo[B](f: F[E, A => B]): F[E, B] =
        implicitly[BiApply[F]].apply(f, self)

      def >*>[B](f: F[E, A => B]): F[E, B] =
        applyTo(f)

    }

    class BiMonadOps_ApplyFlipped[F[_, _]: BiMonad, E, A](self: F[E, A]) {

      def flatMap[B](f: A => F[E, B]): F[E, B] =
        implicitly[BiMonad[F]].flatMap(f, self)

      def flatApplyTo[B](f: F[E, A => F[E, B]]): F[E, B] =
        implicitly[BiMonad[F]].flatApply(f, self)

      def >##>[B](f: A => F[E, B]): F[E, B] =
        flatMap(f)

      def >**>[B](f: F[E, A => F[E, B]]): F[E, B] =
        flatApplyTo(f)

    }

    class BiFlatApplyOps[F[_, _]: BiMonad, E, A, B](f: F[E, A => F[E, B]]) {

      def flatApply(self: F[E, A]): F[E, B] =
        implicitly[BiMonad[F]].flatApply(f, self)

      def <**<(self: F[E, A]): F[E, B] =
        flatApply(self)

    }

  }

}
