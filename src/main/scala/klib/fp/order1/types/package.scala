package klib.fp.order1

package object types {

  // =====| Functor / Apply / Monad |=====

  trait Functor[F[_]] {

    def forEach[A](f: A => Unit, self: F[A]): Unit

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

}
