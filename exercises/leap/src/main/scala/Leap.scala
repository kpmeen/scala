object Leap {
  def leapYear(year: Int): Boolean = {
    def isDivisibleBy(n: Int): Boolean = year % n == 0

    isDivisibleBy(4) && (isDivisibleBy(400) || !isDivisibleBy(100))

  }
}
