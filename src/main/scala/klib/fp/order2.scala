package klib.fp

import scala.language.implicitConversions

object order2 extends order2

trait order2 {

  // TODO (KR) : It might be better to use 'E' ?
  //           : We will see how well it figure out the types

  // =====| Type Classes |=====
  
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
  
  // =====| Usage |=====
  
  
  
}
