package klib

import klib.fp.typeclass.Monad

package object dynamic {

  import scala.language.dynamics
  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox

  trait Message

  sealed trait ??[+T] extends Dynamic {

    // =====| Dynamic |=====

    def selectDynamic(method: String): ??[Any] = macro MyMonadMacro.selectDynamicImpl

    def updateDynamic(method: String)(value: Any): ??[Unit] = macro MyMonadMacro.updateDynamicImpl

    def applyDynamic(method: String)(args: Any*): ??[Any] = macro MyMonadMacro.applyDynamicImpl

  }

  case class Alive[T](value: T, messages: List[Message]) extends ??[T]

  case class Dead(messages: List[Message]) extends ??[Nothing]

  implicit class ToAliveOps[T](value: T) {

    def alive: ??[T] =
      Alive[T](value, Nil)

  }

  object MyMonadMacro {

    def selectDynamicImpl(c: whitebox.Context)(method: c.Tree): c.Tree = {
      import c.universe._
      val q"${methodName: String}" = method
      q"${c.prefix}.map(_.${TermName(methodName).encodedName.toTermName})"
    }

    def updateDynamicImpl(c: whitebox.Context)(method: c.Tree)(value: c.Tree): c.Tree = {
      import c.universe._
      val q"${methodName: String}" = method
      q"${c.prefix}.map(_.${TermName(methodName).encodedName.toTermName} = $value)"
    }

    def applyDynamicImpl(c: whitebox.Context)(method: c.Tree)(args: c.Tree*): c.Tree = {
      import c.universe._
      val q"${methodName: String}" = method
      q"${c.prefix}.map(_.${TermName(methodName).encodedName.toTermName}(..$args))"
    }

  }

  implicit val monad_?? : Monad[??] = new Monad[??] {

    override def flatten[A](self: ??[??[A]]): ??[A] =
      self match {
        case _self: Dead =>
          _self
        case Alive(Dead(msgs1), msgs2) =>
          Dead(msgs1 ::: msgs2)
        case Alive(Alive(value, msgs1), msgs2) =>
          Alive(value, msgs1 ::: msgs2)
      }

    override def lift[A](self: A): ??[A] =
      Alive(self, Nil)

    override def apply[A, B](self: ??[A])(f: ??[A => B]): ??[B] =
      (f, self) match {
        case (Dead(_fMsgs), Dead(_selfMsgs)) =>
          Dead(_selfMsgs ::: _fMsgs)
        case (Dead(_fMsgs), Alive(_, _selfMsgs)) =>
          Dead(_selfMsgs ::: _fMsgs)
        case (Alive(_, _fMsgs), Dead(_selfMsgs)) =>
          Dead(_selfMsgs ::: _fMsgs)
        case (Alive(_f, _fMsgs), Alive(_self, _selfMsgs)) =>
          Alive(_f(_self), _selfMsgs ::: _fMsgs)
      }

    override def forEach[A](self: ??[A])(f: A => Unit): Unit =
      self match {
        case _: Dead =>
        case Alive(_self, _) =>
          f(_self)
      }

    override def map[A, B](self: ??[A])(f: A => B): ??[B] =
      self match {
        case _self: Dead =>
          _self
        case Alive(_self, _selfMsgs) =>
          Alive(f(_self), _selfMsgs)
      }

  }

}
