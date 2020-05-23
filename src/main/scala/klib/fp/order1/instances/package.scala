package klib.fp.order1

import scala.annotation.tailrec

import klib.fp.order1.types._
import klib.handling.Message
import klib.handling.MessageAccumulator
import klib.handling.MessageAccumulator._

package object instances {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def forEach[A](f: A => Unit, self: Option[A]): Unit =
      self match {
        case None =>
          ()
        case Some(_self) =>
          f(_self)
      }

    override def map[A, B](f: A => B, self: Option[A]): Option[B] =
      self match {
        case None =>
          None
        case Some(_self) =>
          Some(f(_self))
      }

    override def lift[A](self: A): Option[A] =
      Some(self)

    override def apply[A, B](f: Option[A => B], self: Option[A]): Option[B] =
      (f, self) match {
        case (None, _) =>
          None
        case (_, None) =>
          None
        case (Some(_f), Some(_self)) =>
          Some(_f(_self))
      }

    override def flatten[A](self: Option[Option[A]]): Option[A] =
      self match {
        case None =>
          None
        case Some(_self) =>
          _self
      }

  }

  implicit val listMonad: Monad[List] = new Monad[List] {

    override def forEach[A](f: A => Unit, self: List[A]): Unit = {
      @tailrec
      def loop(list: List[A]): Unit =
        list match {
          case Nil =>
            ()
          case head :: tail =>
            f(head)
            loop(tail)
        }

      loop(self)
    }

    override def map[A, B](f: A => B, self: List[A]): List[B] = {
      @tailrec
      def loop(_self: List[A], prev: List[B]): List[B] =
        _self match {
          case Nil =>
            prev.reverse
          case head :: tail =>
            loop(tail, f(head) :: prev)
        }

      loop(self, Nil)
    }

    override def lift[A](self: A): List[A] =
      List(self)

    override def apply[A, B](f: List[A => B], self: List[A]): List[B] = {
      @tailrec
      def loop(_f: List[A => B], _self: List[A], prev: List[B]): List[B] =
        _f match {
          case Nil =>
            prev.reverse
          case _fHead :: _fTail =>
            _self match {
              case Nil =>
                loop(_fTail, self, prev)
              case _selfHead :: _selfTail =>
                loop(_f, _selfTail, _fHead(_selfHead) :: prev)
            }
        }

      loop(f, self, Nil)
    }

    override def flatten[A](self: List[List[A]]): List[A] = {
      @tailrec
      def loop(_self: List[List[A]], prev: List[A]): List[A] =
        _self match {
          case Nil =>
            prev.reverse
          case _selfHead :: _selfTail =>
            _selfHead match {
              case Nil =>
                loop(_selfTail, prev)
              case __selfHead :: __selfTail =>
                loop(__selfTail :: _selfTail, __selfHead :: prev)
            }
        }

      loop(self, Nil)
    }

  }

  implicit def messageAccumulatorMonad[E <: Message]: Monad[MessageAccumulator[E, ?]] =
    new Monad[MessageAccumulator[E, *]] {

      override def forEach[A](f: A => Unit, self: MessageAccumulator[E, A]): Unit =
        self match {
          case _: Dead[E] =>
            ()
          case Alive(value, _) =>
            f(value)
        }

      override def map[A, B](f: A => B, self: MessageAccumulator[E, A]): MessageAccumulator[E, B] =
        self match {
          case dead: Dead[E] =>
            dead
          case Alive(value, msgs) =>
            new Alive(f(value), msgs)
        }

      override def lift[A](self: A): MessageAccumulator[E, A] =
        new Alive(self, Nil)

      override def apply[A, B](f: MessageAccumulator[E, A => B], self: MessageAccumulator[E, A]): MessageAccumulator[E, B] =
        f match {
          case Dead(_fMsgs) =>
            self match {
              case Dead(_selfMsgs) =>
                new Dead(_selfMsgs ::: _fMsgs)
              case Alive(_, _selfMsgs) =>
                new Dead(_selfMsgs ::: _fMsgs)
            }
          case Alive(_f, _fMsgs) =>
            self match {
              case Dead(_selfMsgs) =>
                new Dead(_selfMsgs ::: _fMsgs)
              case Alive(_self, _selfMsgs) =>
                new Alive(_f(_self), _selfMsgs ::: _fMsgs)
            }
        }

      override def flatten[A](self: MessageAccumulator[E, MessageAccumulator[E, A]]): MessageAccumulator[E, A] =
        self match {
          case dead: Dead[E] =>
            dead
          case Alive(_self, _selfMsgs) =>
            _self match {
              case Dead(__selfMsgs) =>
                new Dead(__selfMsgs ::: _selfMsgs)
              case Alive(__self, __selfMsgs) =>
                new Alive(__self, __selfMsgs ::: _selfMsgs)
            }
        }

    }

}
