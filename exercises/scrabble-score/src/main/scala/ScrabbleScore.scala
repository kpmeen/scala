object ScrabbleScore {

  private val scores =
    Seq(
      "AEIOULNRST" -> 1,
      "DG" -> 2,
      "BCMP" -> 3,
      "FHVWY" -> 4,
      "K" -> 5,
      "JX" -> 8,
      "QZ" -> 10
    ).flatMap { case (letters, score) => letters.map(_ -> score) }.toMap

  def score(word: String): Int = word.map(c => scores(c.toUpper)).sum

}
