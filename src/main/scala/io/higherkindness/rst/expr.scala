package io.higherkindness.rst

import cats.Show
import cats.syntax.show._

sealed trait Expr extends Product with Serializable
object Expr {
  final case class Const(value: BigDecimal) extends Expr
  final case class Var(name: String) extends Expr

  final case class Neg(x: Expr) extends Expr

  final case class Add(x: Expr, y: Expr) extends Expr
  final case class Sub(x: Expr, y: Expr) extends Expr

  final case class Prod(x: Expr, y: Expr) extends Expr
  final case class Quot(x: Expr, y: Expr) extends Expr

  implicit val showExpr: Show[Expr] = Show.show {
    case Const(c)   => c.toString
    case Var(name)  => name
    case Neg(x)     => s"-(${x.show})"
    case Add(x, y)  => s"(${x.show} + ${y.show})"
    case Sub(x, y)  => s"(${x.show} - ${y.show})"
    case Prod(x, y) => s"${x.show} * ${y.show}"
    case Quot(x, y) => s"${x.show} / ${y.show}"
  }
}
