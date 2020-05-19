package klib

import scala.meta._
import scala.meta.parsers.Parsed

object ScalaMeta {

  /*
  def myImport(input: String): Either[String, String] =
    input.parse[Stat] match {
      case Parsed.Success(t) =>
        t match {
          case Import(value) =>
            value.map {
              _.importees
                .map {
                  case Importee.Wildcard() =>
                    Left("Wildcard imports are not allowed")
                  case a =>
                    Right(a)
                }
                .flatMap(
                )
            }
          case _ =>
            Left("Statement was not an import")
        }
      case Parsed.Error(position, str, exception) =>
        Left(str)
    }

  /*
     TODO (KR) - Research:
               : scalameta compile time code generation
               : sbt compile time code generation
   */

  def main(args: Array[String]): Unit = {

    def test(str: String): Unit = {
      println(s"> $str")
      println(myImport(str))
    }

    test("oops")
    test("Hello there sonny")

    test("import test.a.b, like.this")
    test("import test.a.b; import like.this")
    test("import test.a.{c => CCC}")

  }
   */

}
