import scala.util.parsing.combinator.RegexParsers

object Alphametics {

  type Result = Map[Char, Int]

  def solve(strExpr: String): Option[Result] = {
    AlphameticsParser.parse(strExpr).flatMap { expr =>
      solveExpr(strExpr.filter(_.isLetter).distinct)(expr)
    }
  }

  private[this] def solveExpr(str: String)(bexpr: Dsl.Expr[Boolean]) = {
    val chars = str ++ Seq.fill(10 - str.length)('.')
    chars.permutations
      .map(_.zipWithIndex.toMap.filterKeys(_.isLetter))
      .find(res => bexpr.evaluate(res).getOrElse(false))
  }

  private[this] object AlphameticsParser extends RegexParsers {

    import Dsl._

    def parse(strExpr: String): Option[Expr[Boolean]] =
      parseAll(boolExpr, strExpr).map(Some.apply).getOrElse(None)

    private[this] def boolExpr: Parser[Expr[Boolean]] = {
      intExpr ~ "==" ~ intExpr ^^ {
        case lhs ~ _ ~ rhs => Equals(lhs, rhs)
      }
    }

    private[this] def intExpr: Parser[Expr[Int]] = intOp | argument

    private[this] def intOp =
      argument ~ operator ~ intExpr ^^ {
        case lhs ~ op ~ rhs => op(lhs, rhs)
      }

    private[this] val word = "[A-Z]+".r ^^ Word
    private[this] val number = "[0-9]+".r ^^ (n => Number(n.toInt))
    private[this] val plus = "+" ^^ const(Plus(_, _))
    private[this] val multiply = "*" ^^ const(Multiply(_, _))
    private[this] val powerOf = "^" ^^ const(PowerOf(_, _))

    private[this] val operator = plus | multiply | powerOf
    private[this] val argument = word | number

    private[this] def const[A](a: A)(notUsed: Any): A = a
  }

  object Dsl {

    sealed trait Expr[T] {
      def evaluate(r: Result): Option[T]
    }

    case class Word(value: String) extends Expr[Int] {
      override def evaluate(r: Result): Option[Int] =
        if (r(value(0)) == 0) None
        else Some(value.map(r).mkString.toInt)
    }

    case class Number(value: Int) extends Expr[Int] {
      override def evaluate(r: Result): Option[Int] = Some(value)
    }

    trait Op[A, B] extends Expr[B] {
      val op: (A, A) => B
      val lhs: Expr[A]
      val rhs: Expr[A]

      override def evaluate(r: Result): Option[B] = {
        for {
          l <- lhs.evaluate(r)
          r <- rhs.evaluate(r)
        } yield op(l, r)
      }
    }

    case class Equals[A](lhs: Expr[A], rhs: Expr[A]) extends Op[A, Boolean] {
      override val op: (A, A) => Boolean = (a, b) => a == b
    }

    case class Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Op[Int, Int] {
      override val op: (Int, Int) => Int = (a, b) => a + b
    }

    case class Multiply(lhs: Expr[Int], rhs: Expr[Int]) extends Op[Int, Int] {
      override val op: (Int, Int) => Int = (a, b) => a * b
    }

    case class PowerOf(lhs: Expr[Int], rhs: Expr[Int]) extends Op[Int, Int] {
      override val op: (Int, Int) => Int = (a, b) => math.pow(a, b).toInt
    }

  }

}
