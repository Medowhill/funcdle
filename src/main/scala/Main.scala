import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.parsing.combinator._

sealed trait Expr {
  override def toString: String = this match {
    case X => "x"
    case Num(n) => n.toString
    case Neg(e) => s"-$e"
    case Sqrt(e) => s"√$e"
    case Ln(e) => s"㏑$e"
    case Add(l, r) => s"$l+$r"
    case Sub(l, r) => s"$l-$r"
    case Mul(l, r) => s"$l*$r"
    case Div(l, r) => s"$l/$r"
    case Pow(l, r) => s"$l^$r"
  }
}
@JSExportTopLevel("X")
case object X extends Expr
@JSExportTopLevel("Num")
case class Num(n: Int) extends Expr
@JSExportTopLevel("Neg")
case class Neg(e: Expr) extends Expr
@JSExportTopLevel("Sqrt")
case class Sqrt(e: Expr) extends Expr
@JSExportTopLevel("Ln")
case class Ln(e: Expr) extends Expr
@JSExportTopLevel("Add")
case class Add(l: Expr, r: Expr) extends Expr
@JSExportTopLevel("Sub")
case class Sub(l: Expr, r: Expr) extends Expr
@JSExportTopLevel("Mul")
case class Mul(l: Expr, r: Expr) extends Expr
@JSExportTopLevel("Div")
case class Div(l: Expr, r: Expr) extends Expr
@JSExportTopLevel("Pow")
case class Pow(l: Expr, r: Expr) extends Expr

object Main {
  @JSExportTopLevel("evaluate")
  def evaluate(e: Expr, x: Double): Double = e match {
    case X => x
    case Num(n) => n
    case Neg(e) => -evaluate(e, x)
    case Sqrt(e) => math.sqrt(evaluate(e, x))
    case Ln(e) => math.log(evaluate(e, x))
    case Add(l, r) => evaluate(l, x) + evaluate(r, x)
    case Sub(l, r) => evaluate(l, x) - evaluate(r, x)
    case Mul(l, r) => evaluate(l, x) * evaluate(r, x)
    case Div(l, r) => evaluate(l, x) / evaluate(r, x)
    case Pow(l, r) => math.pow(evaluate(l, x), evaluate(r, x))
  }

  @JSExportTopLevel("pretty")
  def pretty(e: Expr): String = e match {
    case X => "x"
    case Num(n) => n.toString
    case Neg(e) => s"(-${pretty(e)})"
    case Sqrt(e) => s"(√${pretty(e)})"
    case Ln(e) => s"(ln ${pretty(e)})"
    case Add(l, r) => s"(${pretty(l)} + ${pretty(r)})"
    case Sub(l, r) => s"(${pretty(l)} - ${pretty(r)})"
    case Mul(l, r) => s"(${pretty(l)} * ${pretty(r)})"
    case Div(l, r) => s"(${pretty(l)} / ${pretty(r)})"
    case Pow(l, r) => s"(${pretty(l)} ^ ${pretty(r)})"
  }

  @JSExportTopLevel("derivative")
  def derivative(e: Expr): Expr = e match {
    case X => Num(1)
    case Num(n) => Num(0)
    case Neg(e) => Neg(derivative(e))
    case Sqrt(e) => Div(Div(derivative(e), Num(2)), Sqrt(e))
    case Ln(e) => Div(derivative(e), e)
    case Add(l, r) => Add(derivative(l), derivative(r))
    case Sub(l, r) => Sub(derivative(l), derivative(r))
    case Mul(l, r) => Add(Mul(derivative(l), r), Mul(l, derivative(r)))
    case Div(l, r) => Div(Div(Sub(Mul(r, derivative(l)), Mul(l, derivative(r))), r), r)
    case Pow(l, r) =>
      Mul(
        Pow(l, Sub(r, Num(1))),
        Add(
          Mul(r, derivative(l)),
          Mul(Mul(l, Div(Ln(Mul(l, l)), Num(2))), derivative(r))
        )
      )
  }

  val r = new scala.util.Random

  @JSExportTopLevel("rand")
  def rand(): Expr = Expr.parse(rand(5).toString)

  def rand(size: Int): Expr = size match {
    case 1 => if (r.nextBoolean()) Num(r.nextInt(10)) else X
    case 2 =>
      val e = rand(1)
      r.nextInt(3) match {
        case 0 => Neg(e)
        case 1 => Sqrt(e)
        case 2 => Ln(e)
      }
    case 3 =>
      if (r.nextInt(3) == 0) {
        r.nextInt(4) match {
          case 0 => Sqrt(Neg(X))
          case 1 => Sqrt(Sqrt(X))
          case 2 => Ln(Neg(X))
          case 3 => Ln(Sqrt(X))
        }
      } else
        randBin(rand(1), rand(1))
    case 5 =>
      if (r.nextInt(4) == 0)
        randBin(rand(2), rand(2))
      else
        randBin(rand(1), rand(3))
  }

  def randBin(e1: Expr, e2: Expr): Expr = r.nextInt(5) match {
    case 0 => Add(e1, e2)
    case 1 => Sub(e1, e2)
    case 2 => Mul(e1, e2)
    case 3 => Div(e1, e2)
    case 4 => Pow(e1, e2)
  }
}

object Expr extends RegexParsers {

  @JSExportTopLevel("parse")
  def parse(str: String): Expr =
    try {
      parseAll(e0, str).get
    } catch {
      case _: Exception => null
    }

  private lazy val n: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  private lazy val e0: Parser[Expr] =
    e1 ~ rep(("+" | "-") ~ e1) ^^ { case e ~ es => es.foldLeft(e){
      case (l, "+" ~ r) => Add(l, r)
      case (l,  _  ~ r) => Sub(l, r)
    }}

  private lazy val e1: Parser[Expr] =
    e2 ~ rep(("*" | "/") ~ e2) ^^ { case e ~ es => es.foldLeft(e){
      case (l, "*" ~ r) => Mul(l, r)
      case (l,  _  ~ r) => Div(l, r)
    }}

  private lazy val e2: Parser[Expr] =
    rep1sep(e3, "^") ^^ (_.reduceRight(Pow))

  private lazy val e3: Parser[Expr] =
    "-" ~> e3 ^^ Neg |
    "√" ~> e3 ^^ Sqrt |
    "㏑" ~> e3 ^^ Ln |
    e4

  private lazy val e4: Parser[Expr] = "x" ^^^ X | n ^^ Num
}
