object Pangrams {

  def isPangram(input: String): Boolean =
    input.toLowerCase.distinct.sorted.containsSlice('a' to 'z')

}

