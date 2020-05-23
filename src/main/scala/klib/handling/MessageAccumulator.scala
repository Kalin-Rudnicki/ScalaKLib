package klib.handling

import scalaz.Scalaz._

sealed trait MessageAccumulator[+E <: Message, +T] {

  import MessageAccumulator._

  def msgs: List[E]

  def messages: List[E] =
    msgs.reverse

  def <<[E2 >: E <: Message](moreMessages: E2*): MessageAccumulator[E2, T]

  def toOption: Option[T] =
    this match {
      case Alive(value, _) =>
        value.some
      case _ =>
        None
    }

  def sort[E2 >: E <: Message](implicit
      sorter: MessageSorter[E2]
  ): (SortedMessages[E2], Option[T]) = {
    val sorted: SortedMessages[E2] = MessageType.sort(messages)
    (sorted, toOption.flatMap(v => sorted.error.isEmpty.option(v)))
  }

}

object MessageAccumulator {

  // =====| Alive |=====

  final class Alive[E <: Message, T] private[klib] (
      private val value: T,
      val msgs: List[E]
  ) extends MessageAccumulator[E, T] {

    override def <<[E2 >: E <: Message](
        moreMessages: E2*
    ): MessageAccumulator[E2, T] =
      new Alive[E2, T](value, moreMessages.toList.reverse ::: msgs)

    override def toString: String =
      s"Value($value)(${messages.mkString(", ")})"

  }

  object Alive {

    def apply[E <: Message, T](value: T, messages: E*): Alive[E, T] =
      new Alive[E, T](value, messages.toList.reverse)

    def unapply[E <: Message, T](self: Alive[E, T]): Option[(T, List[E])] =
      (self.value, self.msgs).some

  }

  // =====| Dead |=====

  final class Dead[E <: Message] private[klib] (val msgs: List[E]) extends MessageAccumulator[E, Nothing] {

    override def <<[E2 >: E <: Message](
        moreMessages: E2*
    ): MessageAccumulator[E2, Nothing] =
      new Dead[E2](moreMessages.toList.reverse ::: msgs)

    override def toString: String =
      s"NoValue(${messages.mkString(", ")})"

  }

  object Dead {

    def apply[E <: Message](m0: E, messages: E*): Dead[E] =
      new Dead[E]((m0 :: messages.toList).reverse)

    def unapply[E <: Message](self: Dead[E]): Option[List[E]] =
      self.msgs.some

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

  // =====| Monad |=====

}
