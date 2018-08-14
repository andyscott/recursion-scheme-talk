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


  val step: ExprF[BigDecimal] => BigDecimal = {
    case VarF  ("x")   => 10
    case ConstF(v)     => v
    case NegF  (x)     => -x
    case AddF  (x, y)  => x + y
    case SubF  (x, y)  => x - y
    case ProdF (x, y)  => x * y
    case QuotF (x, y)  => x / y
  }

  val unwrap: Expr => ExprF[Expr] = {
    case Var  (name)  => VarF(name)
    case Const(v)     => ConstF(v)
    case Neg  (x)     => NegF(x)
    case Add  (x, y)  => AddF(x, y)
    case Sub  (x, y)  => SubF(x, y)
    case Prod (x, y)  => ProdF(x, y)
    case Quot (x, y)  => QuotF(x, y)
  }

  val evaluate: Expr => BigDecimal =
    expr => step(unwrap(expr).map(evaluate))

  import cats.Functor
  import cats.syntax.functor._

  def hylo[F[_]: Functor, A, B](
    algebra: F[B] => B, coalgebra: A => F[A]
  ): A => B = {
    lazy val f: A => B = a => algebra(coalgebra(a).map(f))
    f
  }

  def cata[F[_]: Functor, A, B](
    algebra: F[B] => B
  )(implicit coalgebra: A => F[A]): A => B =
    hylo(algebra, coalgebra)

  implicit val implicitUnwrap = unwrap

  val evaluate2: Expr => BigDecimal =
    cata(step)

}
