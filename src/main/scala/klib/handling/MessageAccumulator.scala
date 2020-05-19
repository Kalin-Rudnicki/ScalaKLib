package klib.handling

import scalaz.Scalaz._

sealed trait MessageAccumulator[E <: Message, +T] {

  import MessageAccumulator._

  // =====| Implement these... |=====

  def msgs: List[E]

  def messages: List[E] =
    msgs.reverse

  def <<(moreMessages: E*): MessageAccumulator[E, T]

  def map[T2](f: T => T2): MessageAccumulator[E, T2]

  def flatMap[T2](
      f: T => MessageAccumulator[E, T2]
  ): MessageAccumulator[E, T2]

  def forEach(f: T => Unit): Unit

  // =====| ... and you get these for free |=====

  def toOption: Option[T] =
    this match {
      case Alive(_, v) =>
        v.some
      case _ =>
        None
    }

  def sort[E2 >: E <: Message](implicit
      sorter: MessageSorter[E2]
  ): (SortedMessages[E2], Option[T]) = {
    val sorted: SortedMessages[E2] = MessageType.sort(messages)
    (sorted, toOption.flatMap(v => sorted.error.isEmpty.option(v)))
  }

  def <@>[T2](f: T => T2): MessageAccumulator[E, T2] =
    this.map(f)

  def <#>[T2](
      f: T => MessageAccumulator[E, T2]
  ): MessageAccumulator[E, T2] =
    this.flatMap(f)

}

object MessageAccumulator {

  // =====| Value |=====

  final class Alive[E <: Message, T] private[MessageAccumulator] (
      val msgs: List[E],
      private val value: T
  ) extends MessageAccumulator[E, T] {

    override def map[T2](f: T => T2): MessageAccumulator[E, T2] =
      new Alive(msgs, f(value))

    override def forEach(f: T => Unit): Unit =
      f(value)

    override def flatMap[T2](
        f: T => MessageAccumulator[E, T2]
    ): MessageAccumulator[E, T2] =
      f(value) match {
        case Alive(m, v) =>
          new Alive[E, T2](m ::: msgs, v)
        case Dead(m) =>
          new Dead[E](m ::: msgs)
      }

    override def <<(
        moreMessages: E*
    ): MessageAccumulator[E, T] =
      new Alive[E, T](moreMessages.toList.reverse ::: msgs, value)

    override def toString: String =
      s"Value($value)(${messages.mkString(", ")})"

  }

  object Alive {

    def apply[E <: Message, T](value: T, messages: E*): Alive[E, T] =
      new Alive[E, T](messages.toList.reverse, value)

    def unapply[E <: Message, T](a: Alive[E, T]): Option[(List[E], T)] =
      (a.msgs, a.value).some

  }

  // =====| NoValue |=====

  final class Dead[E <: Message] private[MessageAccumulator] (val msgs: List[E]) extends MessageAccumulator[E, Nothing] {

    override def map[T2](f: Nothing => T2): MessageAccumulator[E, T2] =
      this

    override def flatMap[T2](
        f: Nothing => MessageAccumulator[E, T2]
    ): MessageAccumulator[E, T2] =
      this

    override def forEach(f: Nothing => Unit): Unit =
      ()

    override def <<(
        moreMessages: E*
    ): MessageAccumulator[E, Nothing] =
      new Dead[E](moreMessages.toList.reverse ::: msgs)

    override def toString: String =
      s"NoValue(${messages.mkString(", ")})"

  }

  object Dead {

    def apply[E <: Message](m0: E, messages: E*): Dead[E] =
      new Dead[E]((m0 :: messages.toList).reverse)

    def unapply[E <: Message](a: Dead[E]): Option[List[E]] =
      a.msgs.some

  }

  // =====| MessageHandler |=====

  trait MessageSorter[-E <: Message] {

    def sort(e: E): MessageType.Value

  }

  object MessageType extends Enumeration {

    def sort[E <: Message](
        list: List[E]
    )(implicit sorter: MessageSorter[E]): SortedMessages[E] = {
      val sorted: Map[MessageType.Value, List[E]] =
        list.map(m => (sorter.sort(m), m)).groupMap(_._1)(_._2)
      SortedMessages[E](
        sorted.getOrElse(Ignore, Nil),
        sorted.getOrElse(Debug, Nil),
        sorted.getOrElse(Info, Nil),
        sorted.getOrElse(Warning, Nil),
        sorted.getOrElse(Error, Nil)
      )
    }

    // =====| Enums |=====

    val Ignore, Debug, Info, Warning, Error = Value

  }

  final case class SortedMessages[E <: Message](
      ignore: List[E],
      debug: List[E],
      info: List[E],
      warning: List[E],
      error: List[E]
  )

}
