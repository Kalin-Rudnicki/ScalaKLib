package klib

import scala.language.implicitConversions

import klib.Test.MyMaybe._
import klib.fp.all._

object Test {
  
  enum MyMaybe[+T] {
    
    case MyNone extends MyMaybe[Nothing]
    
    case MySome[A](some: A) extends MyMaybe[A]
    
  }
  
  given myMaybeMonad as Monad[MyMaybe] {
    
    def map[A, B](f: A => B, self: MyMaybe[A]): MyMaybe[B] =
      self match {
        case MyNone =>
          MyNone
        case MySome(s) =>
          MySome(f(s))
      }
    
    def lift[A](self: A): MyMaybe[A] =
      MySome(self)
    
    def apply[A, B](f: MyMaybe[A => B], self: MyMaybe[A]): MyMaybe[B] =
      (self, f) match {
        case (MyNone, _) =>
          MyNone
        case (_, MyNone) =>
          MyNone
        case (MySome(sSelf), MySome(sF)) =>
          MySome(sF(sSelf))
      }
    
    def flatten[A](self: MyMaybe[MyMaybe[A]]): MyMaybe[A] =
      self match {
        case MyNone =>
          MyNone
        case MySome(MyNone) =>
          MyNone
        case MySome(MySome(self)) =>
          MySome(self)
      }
    
  }
  
  given optionMonad as Monad[Option] {
    
    def map[A, B](f: A => B, self: Option[A]): Option[B] =
      self match {
        case None =>
          None
        case Some(s) =>
          Some(f(s))
      }
    
    def lift[A](self: A): Option[A] =
      Some(self)
    
    def apply[A, B](f: Option[A => B], self: Option[A]): Option[B] =
      (self, f) match {
        case (None, _) =>
          None
        case (_, None) =>
          None
        case (Some(sSelf), Some(sF)) =>
          Some(sF(sSelf))
      }
    
    def flatten[A](self: Option[Option[A]]): Option[A] =
      self match {
        case None =>
          None
        case Some(MyNone) =>
          None
        case Some(Some(self)) =>
          Some(self)
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
  
  def (i: Int
  ).test(j: Int): Int
  =
  i + j
  
  def [F[_]: Functor, A, B](self: F[A]).fmap(f: A => B): F[B] =
    summon[Functor[F]].map(f, self)
  
  def main(args: Array[String]): Unit = {
    
    val res0 = MySome(4).fmap(_ + 4)
    println(res0)
    
    val res1 = Some(5).asInstanceOf[Option].fmap((a: Int) => a + 6)
    
  }
  
}
