object Etl {

  def transform(in: Map[Int, Seq[String]]): Map[String, Int] = {
    in.flatMap(kv => kv._2.map(letter => letter.toLowerCase -> kv._1))
  }

}
