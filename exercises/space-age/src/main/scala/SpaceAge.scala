object SpaceAge {

  def onEarth(seconds: Double): Double = Earth.planetAge(seconds)
  def onMercury(seconds: Double): Double = Mercury.planetAge(seconds)
  def onVenus(seconds: Double): Double = Venus.planetAge(seconds)
  def onMars(seconds: Double): Double = Mars.planetAge(seconds)
  def onJupiter(seconds: Double): Double = Jupiter.planetAge(seconds)
  def onSaturn(seconds: Double): Double = Saturn.planetAge(seconds)
  def onUranus(seconds: Double): Double = Uranus.planetAge(seconds)
  def onNeptune(seconds: Double): Double = Neptune.planetAge(seconds)

  sealed trait Planet {
    val orbitalPeriod: Double

    val earthYearAsDays: Double = 365.25

    def secondsAsEarthDays(seconds: Double): Double = ((seconds / 60) / 60) / 24

    def orbitalPeriodAsDays: Double = earthYearAsDays * orbitalPeriod

    def planetAge(seconds: Double): Double = {
      ((secondsAsEarthDays(seconds) / orbitalPeriodAsDays) * 100).round / 100.0
    }
  }

  case object Earth extends Planet {
    override val orbitalPeriod: Double = earthYearAsDays
    override val orbitalPeriodAsDays: Double = orbitalPeriod
  }

  case object Mercury extends Planet {
    override val orbitalPeriod: Double = 0.2408467
  }
  case object Venus extends Planet {
    override val orbitalPeriod: Double = 0.61519726
  }
  case object Mars extends Planet {
    override val orbitalPeriod: Double = 1.8808158
  }
  case object Jupiter extends Planet {
    override val orbitalPeriod: Double = 11.862615
  }
  case object Saturn extends Planet {
    override val orbitalPeriod: Double = 29.447498
  }
  case object Uranus extends Planet {
    override val orbitalPeriod: Double = 84.016846
  }
  case object Neptune extends Planet {
    override val orbitalPeriod: Double = 164.79132
  }

}
