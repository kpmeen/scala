object AtbashCipher {

  def replace(c: Char): String = c match {
    case c: Char if c.isDigit  => c.toString
    case c: Char if c.isLetter => ('a' + ('z' - c.toLower)).toChar.toString
    case _                     => ""
  }

  def encode(str: String): String = {
    str
      .foldLeft("")((encoded, c) => s"$encoded${replace(c)}")
      .grouped(5)
      .mkString(" ")
  }

  def decode(str: String): String = {
    str.foldLeft("")((decoded, c) => s"$decoded${replace(c)}")
  }

}
