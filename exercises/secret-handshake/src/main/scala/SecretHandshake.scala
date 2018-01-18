
object SecretHandshake {

  private val ReverseResult = "10000"
  private val BinaryCommands = Map(
    "1" -> "wink",
    "10" -> "double blink",
    "100" -> "close your eyes",
    "1000" -> "jump"
  )

  private implicit def binStrToInt(s: String): Int = Integer.parseInt(s, 2)

  def commands(num: Int): List[String] = {
    val res = BinaryCommands.filter(bc => (bc._1 & num) != 0).values.toList

    if ((num & ReverseResult) != 0) res.reverse
    else res
  }
}
