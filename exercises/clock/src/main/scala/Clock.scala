import scala.annotation.tailrec

class Clock(val minutes: Int) {

  def +(c: Clock) = Clock(minutes + c.minutes)

  def -(c: Clock): Clock = {
    @tailrec
    def foo(m: Int): Int = m match {
      case i: Int if i >= 0 => i
      case i: Int           => foo(i + Clock.MinutesInDay)
    }

    Clock(foo(minutes - c.minutes))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case c: Clock => minutes == c.minutes
      case _        => false
    }
  }

}

object Clock {

  private val MinutesInDay = 24 * 60

  private[this] def normalizeTime(m: Int): Int = m % MinutesInDay match {
    case i: Int if i >= 0 => i
    case i: Int           => MinutesInDay + i
  }

  def apply(hours: Int, minutes: Int): Clock = {
    new Clock(normalizeTime(hours * 60 + minutes))
  }

  def apply(minutes: Int): Clock = {
    new Clock(normalizeTime(minutes))
  }

}
