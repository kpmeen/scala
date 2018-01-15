object Bob {
  def response(statement: String): String = statement match {
    case Shouting() => "Whoa, chill out!"
    case Question() => "Sure."
    case Silence()  => "Fine. Be that way!"
    case _          => "Whatever."
  }

  case object Shouting {
    def unapply(str: String): Boolean =
      str.matches(".*[A-Z].*") && str == str.toUpperCase
  }

  case object Question {
    def unapply(str: String): Boolean = str.trim.endsWith("?")
  }

  case object Silence {
    def unapply(str: String): Boolean = str.trim.isEmpty
  }
}
