package klib.handling

import org.scalactic.source.Position

trait Message {

  def message: String

  def pos: Position

}
