import scala.annotation.tailrec

object BinarySearch {

  def find[T](seq: Seq[T], v: T)(implicit o: T => Ordered[T]): Option[Int] = {
    @tailrec
    def binSearch(dict: Seq[T], arg: T, start: Int, end: Int): Option[Int] = {
      if (start > end || start < 0) None
      else {
        val m = math.floor(0.5 * (start + end)).toInt
        val elem = dict(m)
        if (arg == elem) Some(m)
        else if (arg < elem) binSearch(dict, arg, start, m - 1)
        else binSearch(dict, arg, m + 1, end)
      }
    }

    binSearch(seq, v, 0, seq.size - 1)
  }

}
