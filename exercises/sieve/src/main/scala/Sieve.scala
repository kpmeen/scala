import scala.collection.mutable

object Sieve {

  def primes(max: Int): List[Int] = {
    val numbers = mutable.SortedSet(2 to max: _*)

    (2 to math.sqrt(max).toInt).foreach { n =>
      numbers.find(_ == n).foreach { _ =>
        ((n * n) to max by n).foreach(numbers.remove)
      }
    }

    numbers.toList
  }

}
