import scala.math.BigInt

object Grains {

  case class Square(idx: Int, grains: BigInt)

  private val squares = (1 to 64)
    .foldLeft(List.empty[Square]) { (aggr, squareNum) =>
      aggr match {
        case Nil       => List(Square(squareNum, BigInt(1)))
        case head :: _ => Square(squareNum, head.grains * 2) :: aggr
      }
    }
    .reverse

  def square(idx: Int): Option[BigInt] =
    squares.find(_.idx == idx).map(_.grains)

  def total: BigInt = squares.map(_.grains).sum

}
