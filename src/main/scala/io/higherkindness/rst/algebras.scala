package io.higherkindness.rst

import qq.droste._
import cats.implicits._
import scala.util.Try

object Algebras {
  import Expr._
  import ExprF._

  def evaluateM(variables: Map[String, BigDecimal]): AlgebraM[Either[String, ?], ExprF, BigDecimal] = AlgebraM {
    case VarF  (name)  => variables.get(name).toRight(s"unknown variable: $name")
    case ConstF(v)     => v.asRight
    case NegF  (x)     => (-x).asRight
    case AddF  (x, y)  => (x + y).asRight
    case SubF  (x, y)  => (x - y).asRight
    case ProdF (x, y)  => (x * y).asRight
    case QuotF (x, y)  => Try(x / y).toOption.toRight("division error!")
  }

  def differentiate(wrt: String): RAlgebra[Expr, ExprF, Expr] = RAlgebra {
    case VarF(`wrt`)              => Const(1)
    case _: VarF  [_]             => Const(0)
    case _: ConstF[_]             => Const(0)
    case NegF  ((_, xʹ))          => Neg(xʹ)
    case AddF  ((_, xʹ), (_, yʹ)) => Add(xʹ, yʹ)
    case SubF  ((_, xʹ), (_, yʹ)) => Sub(xʹ, yʹ)
    case ProdF ((x, xʹ), (y, yʹ)) => Add(Prod(x, yʹ), Prod(xʹ, y))
    case QuotF ((x, xʹ), (y, yʹ)) => Quot(Sub(Prod(xʹ, y), Prod(x, yʹ)), Prod(y, y))
  }

}
