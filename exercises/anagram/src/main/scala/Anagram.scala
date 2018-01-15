object Anagram {

  def anagrams(word: String, candidates: List[String]): List[String] = {
    candidates
      .filter(_.toLowerCase.sorted == word.toLowerCase.sorted)
      .filterNot(_.toLowerCase == word.toLowerCase)
  }

}
