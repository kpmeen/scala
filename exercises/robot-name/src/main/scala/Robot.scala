import scala.collection.mutable
import scala.util.Random

object RobotNameGenerator {

  private[this] val chars = 'A' to 'Z'

  private[this] val used = mutable.Set.empty[String]

  def uniqueName: Iterator[String] = {
    def generate(): String = {
      val a = chars.charAt(Random.nextInt(chars.length))
      val b = chars.charAt(Random.nextInt(chars.length))
      val digits = Random.nextInt(1000)

      val name = f"$a$b${digits % 1000}%03d"

      if (used.add(name)) name else generate()
    }

    Iterator.continually(generate())
  }

}

class Robot {
  private[this] var genName = RobotNameGenerator.uniqueName.next

  def name: String = genName

  def reset(): Unit = genName = RobotNameGenerator.uniqueName.next
}
