package klib.handling

import klib.handling.MessageAccumulator.Dead
import org.scalactic.source.Position

trait Message {

  val stackTrace: List[StackTraceElement] =
    Thread.currentThread.getStackTrace.toList.dropWhile { t =>
      t.getLineNumber != pos.lineNumber || t.getFileName != pos.fileName
    }

  def message: String

  protected def pos: Position

  def dead: MessageAccumulator[this.type, Nothing] =
    Dead(this)

  override def toString: String =
    s"$message${stackTrace.map(t => s"\n\t$t").mkString("")}"

}
