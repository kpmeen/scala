import Allergen._

object Allergies {

  val AllAllergens = List(
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats
  )

  def allergicTo(allergen: Allergen, score: Int): Boolean = {
    (allergen.id & score) != 0
  }

  def list(score: Int): List[Allergen] =
    AllAllergens.filter(a => allergicTo(a, score))

}
object Allergen {

  sealed abstract class Allergen(val id: Int) {
    def name: String = getClass.getSimpleName.stripSuffix("$").toLowerCase()
  }

  case object Eggs extends Allergen(1)

  case object Peanuts extends Allergen(2)

  case object Shellfish extends Allergen(4)

  case object Strawberries extends Allergen(8)

  case object Tomatoes extends Allergen(16)

  case object Chocolate extends Allergen(32)

  case object Pollen extends Allergen(64)

  case object Cats extends Allergen(128)

}