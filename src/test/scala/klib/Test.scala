package klib

import klib.fp.all._
import klib.fp.all.ToOps._
import klib.fp.all.Auto._

object Test {

  implicit val eitherBiMonad: BiMonad[Either] =
    new BiMonad[Either] {
      override def map[E, A, B](f: A => B, self: Either[E, A]): Either[E, B] =
        self match {
          case Left(l) =>
            Left(l)
          case Right(r) =>
            Right(f(r))
        }

      override def lift[E, A](self: A): Either[E, A] =
        Right(self)

      override def apply[E, A, B](f: Either[E, A => B], self: Either[E, A]): Either[E, B] =
        (self, f) match {
          case (Left(l), _) =>
            Left(l)
          case (_, Left(l)) =>
            Left(l)
          case (Right(rS), Right(rF)) =>
            Right(rF(rS))
        }

      override def flatten[E, A](self: Either[E, Either[E, A]]): Either[E, A] =
        self match {
          case Left(l) =>
            Left(l)
          case Right(Left(l)) =>
            Left(l)
          case Right(Right(r)) =>
            Right(r)
        }

    }

  def add5(i1: Int): Int =
    i1 + 5

  def add(i1: Int)(i2: Int): Int =
    i1 + i2

  def div(i1: Int)(i2: Int): Either[String, Int] =
    if (i2 == 0)
      Left("Div0")
    else
      Right(i1 / i2)

  def main(args: Array[String]): Unit = {}

}
