package io.higherkindness.rst

import cats.Applicative
import cats.Traverse
import cats.Show
import cats.syntax.all._

import reftree.core._

import qq.droste._
import qq.droste.util.DefaultTraverse

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


sealed trait ExprF[A] extends Product with Serializable
object ExprF {
  import Expr._

  final case class ConstF[A](value: BigDecimal) extends ExprF[A]
  final case class VarF[A](name: String) extends ExprF[A]

  final case class NegF[A](x: A) extends ExprF[A]

  final case class AddF[A](x: A, y: A) extends ExprF[A]
  final case class SubF[A](x: A, y: A) extends ExprF[A]

  final case class ProdF[A](x: A, y: A) extends ExprF[A]
  final case class QuotF[A](x: A, y: A) extends ExprF[A]

  implicit val traverseExprF: Traverse[ExprF] = new DefaultTraverse[ExprF] {
    def traverse[G[_]: Applicative, A, B](fa: ExprF[A])(f: A => G[B]): G[ExprF[B]] = fa match {
      case v: VarF  [B @unchecked] => (v: ExprF[B]).pure[G]
      case c: ConstF[B @unchecked] => (c: ExprF[B]).pure[G]
      case e: NegF  [A]            => f(e.x) map (NegF(_))
      case e: AddF  [A]            => (f(e.x), f(e.y)) mapN (AddF(_, _))
      case e: SubF  [A]            => (f(e.x), f(e.y)) mapN (SubF(_, _))
      case e: ProdF [A]            => (f(e.x), f(e.y)) mapN (ProdF(_, _))
      case e: QuotF [A]            => (f(e.x), f(e.y)) mapN (QuotF(_, _))
    }
  }

  implicit val basisExpr: Basis[ExprF, Expr] = Basis.Default[ExprF, Expr](
    Algebra {
      case VarF  (name)  => Var(name)
      case ConstF(v)     => Const(v)
      case NegF  (x)     => Neg(x)
      case AddF  (x, y)  => Add(x, y)
      case SubF  (x, y)  => Sub(x, y)
      case ProdF (x, y)  => Prod(x, y)
      case QuotF (x, y)  => Quot(x, y)
    },
    Coalgebra {
      case Var  (name)  => VarF(name)
      case Const(v)     => ConstF(v)
      case Neg  (x)     => NegF(x)
      case Add  (x, y)  => AddF(x, y)
      case Sub  (x, y)  => SubF(x, y)
      case Prod (x, y)  => ProdF(x, y)
      case Quot (x, y)  => QuotF(x, y)
    })

  // import to pretend to be a primitive Expr,
  // just for rendering ;)
  object masqueradeAsExpr {

    implicit val varFDerivationConfig: ToRefTree.DerivationConfig[VarF[RefTree]] =
      ToRefTree.DerivationConfig[VarF[RefTree]]
        .rename("Var")

    implicit val constFDerivationConfig: ToRefTree.DerivationConfig[ConstF[RefTree]] =
      ToRefTree.DerivationConfig[ConstF[RefTree]]
        .rename("Const")

    implicit val negFDerivationConfig: ToRefTree.DerivationConfig[NegF[RefTree]] =
      ToRefTree.DerivationConfig[NegF[RefTree]]
        .rename("Neg")

    implicit val addFDerivationConfig: ToRefTree.DerivationConfig[AddF[RefTree]] =
      ToRefTree.DerivationConfig[AddF[RefTree]]
        .rename("Add")

    implicit val subFDerivationConfig: ToRefTree.DerivationConfig[SubF[RefTree]] =
      ToRefTree.DerivationConfig[SubF[RefTree]]
        .rename("Sub")

    implicit val prodFDerivationConfig: ToRefTree.DerivationConfig[ProdF[RefTree]] =
      ToRefTree.DerivationConfig[ProdF[RefTree]]
        .rename("Prod")

    implicit val quotFDerivationConfig: ToRefTree.DerivationConfig[QuotF[RefTree]] =
      ToRefTree.DerivationConfig[QuotF[RefTree]]
        .rename("Quot")
  }
}
