case class WordCount(str: String) {

  lazy val countwords: Map[String, Int] = {
    """(?u)\b[^\s,:\.;]+\b""".r
      .findAllIn(str)
      .map(_.toLowerCase)
      .toSeq
      .groupBy(w => w)
      .mapValues(_.length)
  }

}
