import scala.annotation.tailrec

class Accumulate {

  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] = {
    @tailrec
    def inner(l: List[A], agg: List[B]): List[B] = {
      l match {
        case Nil          => agg
        case head :: tail => inner(tail, agg :+ f(head))
      }
    }
    inner(list, List.empty)
  }

//  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] = {
//    list match {
//      case Nil => Nil
//      case head :: tail => f(head) :: accumulate(f, tail)
//    }
//  }

}
