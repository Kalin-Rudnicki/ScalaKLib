package klib

import klib.fp.instances.{given _}
import klib.fp.ops.{given _}

object Test {

  def main(args: Array[String]): Unit = {
    
    List(1, 2, 3)._map(_ + 4)
    val o: Option[Int] = 4.lift
    
  }

}
