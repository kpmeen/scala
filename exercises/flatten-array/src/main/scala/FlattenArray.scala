object FlattenArray {

  def flatten(l: List[Any]): List[Any] = {
    val n = l match {
      case Nil                     => Nil
      case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
      case head :: tail            => head::flatten(tail)
    }
    n.filterNot(_ == null)
  }

}
