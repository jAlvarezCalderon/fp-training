sealed abstract class Even(val value: Int)
case object Zero extends Even(0)
case class Next(previousEvent: Even) extends Even(2 + previousEvent.value)