import scala.annotation.tailrec

object Strain {

  @tailrec
  def fold[T](s: Seq[T], f: T => Boolean, acc: Seq[T] = Seq.empty): Seq[T] = {
    s match {
      case Nil                     => acc
      case head :: tail if f(head) => fold(tail, f, acc :+ head)
      case _ :: tail               => fold(tail, f, acc)
    }
  }

  def keep[T](s: Seq[T], f: T => Boolean): Seq[T] = {
    fold(s, f)
  }

  def discard[T](l: Seq[T], f: T => Boolean): Seq[T] = {
    val neg: T => Boolean = t => !f(t)
    fold(l, neg)
  }

}
