object Hamming {

  def distance(arg1: String, arg2: String): Option[Int] = {
    if (arg1.length != arg2.length) None
    else {
      Some(arg1.toSeq.zip(arg2.toSeq).foldLeft(0) {
        case (dist, (a, b)) if a != b => dist + 1
        case (dist, _) => dist
      })
    }
  }

}
