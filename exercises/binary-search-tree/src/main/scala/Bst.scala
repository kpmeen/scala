case class Bst[+T](
    value: T,
    left: Option[Bst[T]] = None,
    right: Option[Bst[T]] = None
) {

  def insert[U >: T](x: U)(implicit ev: U => Ordered[U]): Bst[U] = {
    def innerIns(x: U, node: Option[Bst[U]]): Option[Bst[U]] =
      node.map(n => n.insert(x)).orElse(Option(Bst(x)))

    if (x <= value) Bst(value, innerIns(x, left), right)
    else Bst(value, left, innerIns(x, right))
  }
}

object Bst {

  def fromList[T](l: List[T])(implicit ev: T => Ordered[T]): Bst[T] =
    l match {
      case Nil      => throw new IllegalArgumentException("The tree cannot be empty")
      case x :: Nil => Bst(x, None, None)
      case x :: xs  => xs.foldLeft(Bst(x, None, None))((r, e) => r.insert(e))
    }

  def toList[T](tree: Bst[T]): List[T] = toList(Some(tree))

  private def toList[T](tree: Option[Bst[T]]): List[T] =
    tree match {
      case Some(b) => toList(b.left) ++ List(b.value) ++ toList(b.right)
      case None    => List.empty
    }
}
