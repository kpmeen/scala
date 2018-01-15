object Sublist {

  def sublist[T](a: List[T], b: List[T]): Result = {
    if (a.equals(b)) Equal
    else if (a.containsSlice(b)) Superlist
    else if (b.containsSlice(a)) Sublist
    else Unequal
  }

  sealed trait Result

  case object Equal extends Result

  case object Unequal extends Result

  case object Sublist extends Result

  case object Superlist extends Result

}
