object PerfectNumbers {

  def classify(num: Int): Either[String, NumberType.NumType] = {
    if (num <= 0) Left("Classification is only possible for natural numbers.")
    else {
      (1 until num).foldLeft(0)((acc, n) => if (num % n == 0) acc + n else acc) match {
        case i: Int if i > num => Right(NumberType.Abundant)
        case i: Int if i < num => Right(NumberType.Deficient)
        case _                 => Right(NumberType.Perfect)
      }

    }
  }

}

object NumberType {

  sealed trait NumType

  case object Perfect extends NumType

  case object Abundant extends NumType

  case object Deficient extends NumType

}
