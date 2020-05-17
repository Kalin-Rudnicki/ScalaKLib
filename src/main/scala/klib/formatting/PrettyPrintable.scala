package klib.formatting

// TODO (KR) : Make this work with scalameta
//           : I could probably do this with regular runtime reflection

trait PrettyPrintable {

  import PrettyPrintable._

  def prettyPrint(indentStr: String = defaultIndent, markerStr: String = defaultMarker): String = {
    val builder: StringBuilder = new StringBuilder
    prettyPrint(indentStr, markerStr, "", builder)
    builder.toString
  }

  def prettyPrint(indentStr: String, markerStr: String, indent: String, builder: StringBuilder): Unit

}

object PrettyPrintable {

  private val defaultIndent = "|   "
  private val defaultMarker = "|-> "

  implicit def prettyPrint[T: ExtPrettyPrintable](value: T): String = {
    val builder: StringBuilder = new StringBuilder
    implicitly[ExtPrettyPrintable[T]].prettyPrint(value, defaultIndent, defaultMarker, "", builder)
    builder.toString
  }

  trait ExtPrettyPrintable[T] {

    def prettyPrint(value: T, indentStr: String, markerStr: String, indent: String, builder: StringBuilder): Unit

  }

}
