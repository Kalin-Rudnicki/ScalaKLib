package klib.fp

import klib.fp.typeclass._

object ops {
  
  export FunctorOpsB.{given _}
  
  // TODO (KR) : Old syntax that I would like to replace
  
  extension FunctorOps on [F[_]: Functor, A](self: F[A]) {

    def forEach(f: A => Unit): Unit =
      summon[Functor[F]].forEach(self)(f)

    def _forEach(f: A => Unit): Unit =
      forEach(f)

  }
  
  extension FunctorOpsB on [F[_]: Functor, A, B](self: F[A]) {
    
    def map(f: A => B): F[B] =
      summon[Functor[F]].map(self)(f)
    
    def _map(f: A => B): F[B] =
      map(f)
    
    def >#>(f: A => B): F[B] =
      map(f)
    
  }

  implicit class ToApplicativeOps[A](self: A) {

    def lift[F[_]: Applicative]: F[A] =
      summon[Applicative[F]].lift(self)

    def _lift[F[_]: Applicative]: F[A] =
      lift

    def ^[F[_]: Applicative] : F[A] =
      lift

  }

  extension ApplicativeOps on [F[_]: Applicative, A, B](f: F[A => B]) {

    def apply(self: F[A]): F[B] =
      implicitly[Applicative[F]].apply(self)(f)

    def _apply(self: F[A]): F[B] =
      apply(self)

    def <*<(self: F[A]): F[B] =
      apply(self)

  }

  extension ApplicativeOpsFlipped on [F[_]: Applicative, A, B](self: F[A]) {

    def applyTo(f: F[A => B]): F[B] =
      implicitly[Applicative[F]].apply(self)(f)

    def _applyTo(f: F[A => B]): F[B] =
      applyTo(f)

    def >*>(f: F[A => B]): F[B] =
      applyTo(f)

  }

  extension MonadOps_ApplicativeFlipped on [F[_]: Monad, A, B](self: F[A]) {

    def flatMap(f: A => F[B]): F[B] =
      implicitly[Monad[F]].flatMap(self)(f)

    def _flatMap(f: A => F[B]): F[B] =
      flatMap(f)

    def flatApplyTo(f: F[A => F[B]]): F[B] =
      implicitly[Monad[F]].flatApply(self)(f)

    def _flatApplyTo(f: F[A => F[B]]): F[B] =
      flatApplyTo(f)

    def >##>(f: A => F[B]): F[B] =
      flatMap(f)

    def >**>(f: F[A => F[B]]): F[B] =
      flatApplyTo(f)

  }

  extension FlatApplicativeOps on [F[_]: Monad, A, B](f: F[A => F[B]]) {

    def flatApply(self: F[A]): F[B] =
      implicitly[Monad[F]].flatApply(self)(f)

    def _flatApply(self: F[A]): F[B] =
      flatApply(self)

    def <**<(self: F[A]): F[B] =
      flatApply(self)

  }

  extension FoldOps on [F[_]: Foldable, A, B](self: F[A]) {

    def fold(_0: B)(join: (A, B) => B): B =
      implicitly[Foldable[F]].fold(self, _0)(join)

    def _fold(_0: B)(join: (A, B) => B): B =
      fold(_0)(join)

  }

  implicit class InvertOps[
      F[_]: Foldable: ZeroAdd,
      G[_]: Applicative,
      A
  ](
      self: F[G[A]]
  )(implicit
      foldableF: Foldable[F],
      zeroAddF: ZeroAdd[F],
      applyG: Applicative[G]
  ) {

    def invert: G[F[A]] =
      foldableF
        .fold[G[A], G[F[A]]](
          self,
          applyG.lift(zeroAddF._0)
        ) { (ga, gfa) =>
          applyG.apply(ga) {
            applyG.map[F[A], A => F[A]](gfa) { (fa: F[A]) => (a: A) =>
              zeroAddF.add(fa, a)
            }
          }
        }

    def _invert: G[F[A]] =
      invert

    def invertR(implicit reversible: Reversible[F]): G[F[A]] =
      applyG.map[F[A], F[A]](invert)(i => implicitly[Reversible[F]].reverse(i, zeroAddF._0))

    def _invertR(implicit reversible: Reversible[F]): G[F[A]] =
      invertR

  }

}
