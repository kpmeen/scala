import scala.util.parsing.combinator.RegexParsers

object Wordy {

  def answer(str: String): Option[Int] = Parser.parse(str)

  object Parser extends RegexParsers {

    private[this] val prefix = "What is "
    private[this] val qmark = "?"
    private[this] val ops =
      "plus" | "minus" | "multiplied by" | "divided by" | "raised to the"

    private[this] def operation = expr ~ rep(ops ~ expr) ^^ {
      case op ~ list =>
        list.foldLeft(op) {
          case (x, "plus" ~ y)          => Sum(x, y)
          case (x, "minus" ~ y)         => Diff(x, y)
          case (x, "multiplied by" ~ y) => Multiply(x, y)
          case (x, "divided by" ~ y)    => Divide(x, y)
          case (x, "raised to the" ~ y) => Pow(x, y)
        }
    }

    private[this] def operand: Parser[Const] =
      """-?\d+""".r ^^ (s => Const(s.toInt))

    private[this] def expr: Parser[Expr] = operand | operation

    private[this] def question = prefix ~> operation <~ qmark

    def parse(str: String): Option[Int] = parseAll(question, str) match {
      case Success(r, _) => Some(r.evaluate())
      case _             => None
    }
  }

  sealed abstract class Expr {
    def evaluate(): Int
  }

  case class Const(v: Int) extends Expr {
    override def evaluate(): Int = v
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def evaluate(): Int = e1.evaluate() + e2.evaluate()
  }

  case class Diff(e1: Expr, e2: Expr) extends Expr {
    override def evaluate(): Int = e1.evaluate() - e2.evaluate()
  }

  case class Multiply(e1: Expr, e2: Expr) extends Expr {
    override def evaluate(): Int = e1.evaluate() * e2.evaluate()
  }

  case class Divide(e1: Expr, e2: Expr) extends Expr {
    override def evaluate(): Int = e1.evaluate() / e2.evaluate()
  }

  case class Pow(e1: Expr, e2: Expr) extends Expr {
    override def evaluate(): Int =
      math.pow(e1.evaluate().toDouble, e2.evaluate().toDouble).toInt
  }

}
