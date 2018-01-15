object Raindrops {

  def convert(n: Int): String =
    Map(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")
      .filter(n % _._1 == 0)
      .toSeq
      .map(_._2)
      .mkString match {
        case "" => n.toString
        case s  => s
      }

}
