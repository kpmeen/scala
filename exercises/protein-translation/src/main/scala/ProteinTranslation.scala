object ProteinTranslation {

  private val StopValues = Seq("UAA", "UAG", "UGA")

  private[this] val dictionary = Map(
    "AUG" -> "Methionine",
    "UUUUUC" -> "Phenylalanine",
    "UUAUUG" -> "Leucine",
    "UCUUCCUCAUCG" -> "Serine",
    "UAUUAC" -> "Tyrosine",
    "UGUUGC" -> "Cysteine",
    "UGG" -> "Tryptophan",
    StopValues.mkString -> "STOP"
  )

  def translate(rna: String): Seq[String] = {
    rna
      .grouped(3)
      .toList
      .takeWhile(s => !StopValues.contains(s))
      .foldLeft(Seq.empty[String]) { (res, rna) =>
        dictionary
          .filterKeys(k => k.grouped(3).contains(rna))
          .values
          .headOption
          .map {
            case v: String if v == "STOP" => res
            case v: String                => res :+ v
          }
          .getOrElse(res)
      }
  }

}
