case class Binary(str: String) {

  val toDecimal: Int = str
    .foldLeft[Option[Int]](Some(0)) {
      case (Some(agr), '0') => Some(agr * 2)
      case (Some(agr), '1') => Some(agr * 2 + 1)
      case _                => None
    }
    .getOrElse(0)

}
