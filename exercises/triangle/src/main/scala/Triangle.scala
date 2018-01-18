case class Triangle(a: Double, b: Double, c: Double) {

  def equilateral: Boolean = validTriangle && (a == b && b == c)

  def isosceles: Boolean = validTriangle && (a == b || a == c || b == c)

  def scalene: Boolean = validTriangle && !isosceles && !equilateral

  private def nonsense: Boolean = Seq(a, b, c).count(_ <= 0) > 0

  private def validTriangle: Boolean = {
    !nonsense && (a + b >= c && a + c >= b && b + c >= a)
  }
}
