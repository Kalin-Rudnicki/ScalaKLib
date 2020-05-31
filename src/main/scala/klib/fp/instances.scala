package klib.fp

import scala.annotation.tailrec

import klib.fp.typeclass._
import klib.handling.MessageAccumulator._
import klib.handling._

trait instances {

  // =====| Option |=====

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def forEach[A](self: Option[A])(f: A => Unit): Unit =
      self match {
        case None =>
          ()
        case Some(_self) =>
          f(_self)
      }

    override def map[A, B](self: Option[A])(f: A => B): Option[B] =
      self match {
        case None =>
          None
        case Some(_self) =>
          Some(f(_self))
      }

    override def lift[A](self: A): Option[A] =
      Some(self)

    override def apply[A, B](self: Option[A])(f: Option[A => B]): Option[B] =
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

  // =====| List |=====

  implicit val listMonad: Monad[List] = new Monad[List] {

    override def forEach[A](self: List[A])(f: A => Unit): Unit = {
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

    override def map[A, B](self: List[A])(f: A => B): List[B] = {
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

    override def apply[A, B](self: List[A])(f: List[A => B]): List[B] = {
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

  implicit val listFoldable: Foldable[List] = new Foldable[List] {

    // TODO (KR) : Write ops
    @tailrec
    override def fold[A, B](self: List[A], _0: B)(join: (A, B) => B): B =
      self match {
        case Nil =>
          _0
        case head :: tail =>
          fold(tail, join(head, _0))(join)
      }

  }

  implicit val listZeroAdd: ZeroAdd[List] = new ZeroAdd[List] {

    override def _0[A]: List[A] = Nil

    override def add[A](self: List[A], that: A): List[A] =
      that :: self

  }

  implicit val listReversible: Reversible[List] = new Reversible[List] {

    @tailrec
    override def reverse[A](next: List[A], prev: List[A]): List[A] =
      next match {
        case Nil =>
          prev
        case head :: tail =>
          reverse(tail, head :: prev)
      }

  }

  // =====| Map |=====
  
  given [K] as Functor[[X] =>> Map[K, X]] {

      override def forEach[A](self: Map[K, A])(f: A => Unit): Unit = {
        @tailrec
        def loop(next: List[A]): Unit =
          next match {
            case Nil =>
              ()
            case head :: tail =>
              f(head)
              loop(tail)
          }

        // TODO (KR) : Optimize?
        loop(self.values.toList)
      }

      override def map[A, B](self: Map[K, A])(f: A => B): Map[K, B] = {
        @tailrec
        def loop(next: List[(K, A)], prev: List[(K, B)]): Map[K, B] =
          next match {
            case Nil =>
              prev.toMap
            case head :: tail =>
              loop(tail, (head._1, f(head._2)) :: prev)
          }

        loop(self.toList, Nil)
      }

    }

  // =====| MessageAccumulator |=====
  
  
  
  given [E <: Message] as Monad[[X] =>> MessageAccumulator[E, X]] {

      override def forEach[A](self: MessageAccumulator[E, A])(f: A => Unit): Unit =
        self match {
          case _: Dead[E] =>
            ()
          case Alive(value, _) =>
            f(value)
        }

      override def map[A, B](self: MessageAccumulator[E, A])(f: A => B): MessageAccumulator[E, B] =
        self match {
          case dead: Dead[E] =>
            dead
          case Alive(value, msgs) =>
            new Alive(f(value), msgs)
        }

      override def lift[A](self: A): MessageAccumulator[E, A] =
        new Alive(self, Nil)

      override def apply[A, B](self: MessageAccumulator[E, A])(f: MessageAccumulator[E, A => B]): MessageAccumulator[E, B] =
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

object instances extends instances
