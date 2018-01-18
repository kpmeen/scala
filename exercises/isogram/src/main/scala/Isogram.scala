object Isogram {

  def isIsogram(str: String): Boolean = {
    !str.toLowerCase
      .filter(_.isLetter)
      .groupBy(identity)
      .values
      .exists(_.length > 1)
  }

}
