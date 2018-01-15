
object PhoneNumber {

  private val PhoneNumRegex = """1?.*?([2-9]\d{2}).*?([2-9]\d{2}).*?(\d{4})""".r


  def clean(phoneNumber: String): Option[String] = {
    val digits = phoneNumber.filter(_.isDigit)
    if (digits.length > 10 && !digits.startsWith("1")) None
    else {
      val res = PhoneNumRegex.findAllMatchIn(phoneNumber).toList.flatMap { m =>
        if (m.groupCount == 3) Some(s"${m.group(1)}${m.group(2)}${m.group(3)}")
        else None
      }
      res.headOption
    }
  }

}
