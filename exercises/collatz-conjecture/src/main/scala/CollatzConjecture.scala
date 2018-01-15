import scala.annotation.tailrec

object CollatzConjecture {

  def steps(num: Int): Option[Int] = {
    @tailrec
    def inner(n: Int, stepNum: Int): Option[Int] = {
      if (num <= 0) None
      else if (n == 1) Some(stepNum)
      else {
        n % 2 match {
          case 0 => inner(n / 2, stepNum + 1)
          case _ => inner((n * 3) + 1, stepNum + 1)
        }
      }
    }
    inner(num, 0)
  }

}
