package klib.fp.utils

extension EitherOps on [A](self: A) {
  
  def left: Either[A, Nothing] =
    Left(self)
  
  def right: Either[Nothing, A] =
    Right(self)
  
}
