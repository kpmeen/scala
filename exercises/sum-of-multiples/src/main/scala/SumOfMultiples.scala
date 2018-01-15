object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    val m = (1 until limit).filter { n =>
      factors.exists { f =>
        n % f match {
          case i: Int if i == 0 => true
          case _ => false
        }
      }
    }
    m.sum
  }
}

