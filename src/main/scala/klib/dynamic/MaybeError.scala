package klib.dynamic

import scala.language.dynamics
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

import MaybeError._

trait Error

sealed abstract class ??[T: TypeTag] extends Dynamic {

  def selectDynamic(name: String): ??[Any] =
    this match {
      case e: Dead =>
        e.asInstanceOf[??[Any]]
      case a @ Alive(value: T, _) =>
        val m = runtimeMirror(value.getClass.getClassLoader)
        val symbol = typeOf[T].decl(TermName(name)).asTerm
        // val im = m.reflect(value)
        // val field = im.reflectField(symbol)
        a.asInstanceOf[??[Any]]
    }

}

object MaybeError {

  final class Alive[T: TypeTag] private (private val value: T, private val errors: List[Error]) extends ??[T] {
    override def toString: String =
      s"Alive[${typeOf[T]}]($value)(${errors.mkString(", ")})"
  }

  object Alive {

    def apply[T: TypeTag](value: T, errors: Error*): Alive[T] =
      new Alive[T](value, errors.toList)

    def unapply[T: TypeTag](arg: Alive[T]): Option[(T, List[Error])] =
      Some((arg.value, arg.errors))

  }

  final class Dead private (private val errors: List[Error]) extends ??[Nothing] {
    override def toString: String =
      s"Dead(${errors.mkString(", ")})"
  }

  object Dead {

    def apply(error0: Error, errors: Error*): Dead =
      new Dead(error0 :: errors.toList)

    def unapply(arg: Dead): Option[List[Error]] =
      Some(arg.errors)

  }

}

object Test {

  case class Person(val firstName: String, val lastName: String, val age: Int)

  def main(args: Array[String]): Unit = {

    val res0: ??[Person] = Alive(Person("Kalin", "Rudnicki", 21))
    println(res0)
    println(res0.firstName)

  }

}
