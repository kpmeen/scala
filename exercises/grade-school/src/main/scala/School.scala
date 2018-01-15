import scala.collection.immutable.SortedMap

class School {

  type DB = Map[Int, Seq[String]]

  private[this] var database: DB = Map.empty

  def add(name: String, g: Int): Unit = database = db.updated(g, grade(g) :+ name)

  def db: DB = database

  def grade(g: Int): Seq[String] = db.getOrElse(g, Seq.empty)

  def sorted: DB = SortedMap(db.toSeq: _*).mapValues(_.sorted)

}
