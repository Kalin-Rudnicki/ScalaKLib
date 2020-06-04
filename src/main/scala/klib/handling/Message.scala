package klib.handling

import klib.handling.MessageAccumulator.Dead

trait Message {

  val stackTrace: List[StackTraceElement] =
    Thread.currentThread.getStackTrace.toList

  def message: String

  def dead: MessageAccumulator[this.type, Nothing] =
    Dead(this)

  override def toString: String =
    s"$message${stackTrace.map(t => s"\n\t$t").mkString("")}"

}
