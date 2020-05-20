package klib.fp

object all extends order1 with order2 {

  object ToOps extends order1.ToOps1 with order2.ToOps2

  object Auto extends order1.Auto1 with order2.Auto2

}
