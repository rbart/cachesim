package cachesim



sealed abstract class Result {
  def time: Long
  def next: Option[Result]
  def block: Block
}

case class Hit(val time: Long, val block: Block) extends Result {
  def next = None
}

case class Miss(val time: Long, val block: Block, val next: Option[Result]) extends Result