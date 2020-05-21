package klib.fp

import scala.language.implicitConversions

object order1 extends order1 {

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

}
