package klib

import klib.fp._
import klib.fp.implicits._
import klib.fp.implicits.auto._

object Test {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def map[A, B](self: Option[A], f: A => B): Option[B] =
      self match {
        case None =>
          None
        case Some(a) =>
          Some(f(a))
      }

    override def lift[A](self: A): Option[A] =
      Some(self)

    override def apply[A, B](self: Option[A], f: Option[A => B]): Option[B] =
      (self, f) match {
        case (None, _) =>
          None
        case (_, None) =>
          None
        case (Some(a), Some(_f)) =>
          Some(_f(a))
      }

    override def flatten[A](self: Option[Option[A]]): Option[A] =
      self match {
        case None =>
          None
        case Some(None) =>
          None
        case Some(Some(a)) =>
          Some(a)
      }
  }

  import MyOption._

  implicit val optionMyOptionFlipper: MonadFlipper_1_1[Option, MyOption] = new MonadFlipper_1_1[Option, MyOption] {

    override def flip[A](self: Option[MyOption[A]]): MyOption[Option[A]] =
      self match {
        case None =>
          MyNone
        case Some(MyNone) =>
          MyNone
        case Some(MySome(a)) =>
          MySome(Some(a))
      }

  }

  sealed trait MyOption[+A]

  object MyOption {

    case object MyNone extends MyOption[Nothing]

    case class MySome[A](a: A) extends MyOption[A]

  }

  def curriedAdd(i1: Int)(i2: Int): Int =
    i1 + i2

  def add(i1: Int, i2: Int): Int =
    i1 + i2

  def curriedDiv(i1: Int)(i2: Int): Option[Int] =
    if (i2 == 0)
      None
    else
      i1 / i2

  def div(i1: Int, i2: Int): Option[Int] =
    if (i2 == 0)
      None
    else
      i1 / i2

  def main(args: Array[String]): Unit = {

    val res0: Option[Int] = 5
    val res1: Option[Int] = (curriedAdd _).lift <* 4 <* 6
    val res2: Option[Int] = (curriedDiv _).lift <<* Some(Some(4)) <** 4

    println(res0)
    println(res1)
    println(res2)

    val res10: Option[MyOption[Int]] = Some(MySome(5))
    val res11: MyOption[Option[Int]] = res10.flip

  }

}
