package klib.fp

trait MonadFlipper_1_1[F[_], G[_]] {

  def flip[A](self: F[G[A]]): G[F[A]]

}
