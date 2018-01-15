object RnaTranscription {

  private val DnaRnaMap = Map(
    'G' -> 'C',
    'C' -> 'G',
    'T' -> 'A',
    'A' -> 'U'
  )

  private def hasInvalidChars(str: String): Boolean =
    !str.toUpperCase.forall(c => DnaRnaMap.contains(c))

  def toRna(str: String): Option[String] = {
    if (hasInvalidChars(str)) None
    else Option(str.toUpperCase.map(c => DnaRnaMap(c)))
  }

}
