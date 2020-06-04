package klib.fp.utils

extension OptionOps on [A](self: A) {
  
  def some: Option[A] =
    Some(self)

}
