package io.higherkindness.rst

import cats._
import cats.effect._
import cats.implicits._

import reftree.core._
import reftree.contrib.SimplifiedInstances._
import qq.droste.reftree.prelude._
import qq.droste._
import qq.droste.data._
import qq.droste.data.prelude._

import Expr._
import ExprF._

object RenderingImplicits {
  implicit val bigDecimalToRefTree: ToRefTree[BigDecimal] =
    ToRefTree(v =>
      RefTree.Ref(v, Seq.empty).rename(v.toString))

  implicit val refTreeToRefTree: ToRefTree[RefTree] =
    ToRefTree(v => v)
}

import RenderingImplicits._

object Main {

  def main(args: Array[String]): Unit =
    main(args.toList).unsafeRunSync

  def main(args: List[String]): IO[Unit] =
    for {
      _ <- FileIO.deleteRecursively(ImgIO.outputPath.toFile)
      _ <- Barberry.check
      //_ <- Aalii.render
      //_ <- Barberry.render
      //_ <- Cohosh.render
      _ <- Deutzia.render
    } yield ()

}

object Aalii {
  import qq.droste.reftree.prelude._

  def render: IO[Unit] = for {
    _ <- ImgIO.render(exprA, "aalii-expr-a")
    _ <- ImgIO.render(exprB, "aalii-expr-b")
    _ <- ImgIO.render(exprC, "aalii-expr-c")
  } yield ()

  // 2x + 1
  lazy val exprA: Expr =
    Add(
      Prod(
        Const(2),
        Var("x")),
      Const(1))

  // (2x + 1)/8
  lazy val exprB: Expr =
    Quot(
      Add(
        Prod(
          Const(2),
          Var("x")),
        Const(1)),
      Const(8))

  // (x - y)(x + y)
  lazy val exprC: Expr =
    Prod(
      Sub(
        Var("x"),
        Var("y")),
      Add(
        Var("x"),
        Var("y")))
}

object Barberry {
  import ExprF.masqueradeAsExpr._

  def render: IO[Unit] = for {
    //_ <- steps.traverseWithIndexM((subSteps, i) =>
      //ImgIO.render(subSteps, s"barberry-$i"))
    //_ <- steps.sliding(2).toList.traverseWithIndexM((subSteps, i) =>
      //ImgIO.render(subSteps, s"barberry-$i-${i + 1}"))
    _ <- ImgIO.render(annotatedResults.toOption.get, s"barberry-annotated-results") // .get = yolo
    _ <- ImgIO.render(eval2, s"barberry-partial-result")
    _ <- ImgIO.render(steps, s"barberry-all")
  } yield ()

  lazy val steps: List[Free[ExprF, BigDecimal]] = List(
    eval0,
    eval1,
    eval2,
    eval3,
    eval4,
    eval5)

  lazy val expected = BigDecimal("2.625")

  lazy val eval0: Free[ExprF, BigDecimal] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.roll(ProdF(
          Free.roll(ConstF(2)),
          Free.roll(VarF("x"))
        )),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval1: Free[ExprF, BigDecimal] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.roll(ProdF(
          Free.roll(ConstF(2)),
          Free.pure(10)
        )),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval2: Free[ExprF, BigDecimal] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.pure(20),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval3: Free[ExprF, BigDecimal] =
    Free.roll(QuotF(
      Free.pure(21),
      Free.roll(ConstF(8))
    ))

  lazy val eval4: Free[ExprF, BigDecimal] =
    Free.roll(QuotF(
      Free.pure(21),
      Free.pure(8)
    ))

  lazy val eval5: Free[ExprF, BigDecimal] =
    Free.pure(expected)

  def attributeCataM[M[_]: Monad, A](
    algebra: AlgebraM[M, ExprF, A]
  ): Expr => M[Cofree[ExprF, A]] =
    scheme.cataM(TransM((fa: ExprF[Cofree[ExprF, A]]) =>
      algebra(fa.map(_.head)).map(a => EnvT(a, fa))).algebra)

  lazy val annotatedResults: Either[String, Cofree[ExprF, BigDecimal]] =
    attributeCataM(Algebras.evaluateM(Map("x" -> 10)))
      .apply(Aalii.exprB)

  def fused[M[_]: Applicative, F[_], A](
    algebra: AlgebraM[M, F, A]
  ): AlgebraM[M, CoenvT[A, F, ?], A] =
    AlgebraM(CoenvT.un(_).fold(_.pure[M], algebra.run))

  def check: IO[Unit] = {
    val f: Free[ExprF, BigDecimal] => Either[String, BigDecimal] =
      scheme.cataM(fused(Algebras.evaluateM(Map("x" -> 10))))

    steps
      .map(f)
      .traverseWithIndexM { (r, i) =>
        val msg = s"barberry.eval$i: $r == Right($expected)"
        IO(assert(r == Right(expected), msg)) *> StdIO.println(s" ? $msg")
      }
      .void
  }
}


object Cohosh {
  import ExprF.masqueradeAsExpr._

  def render: IO[Unit] = for {
    //_ <- steps.traverseWithIndexM((subSteps, i) =>
      //ImgIO.render(subSteps, s"cohosh-$i"))
    //_ <- steps.sliding(2).toList.traverseWithIndexM((subSteps, i) =>
      //ImgIO.render(subSteps, s"cohosh-$i-${i + 1}"))
    _ <- ImgIO.render(steps, s"cohosh-all")
  } yield ()

  lazy val steps: List[Free[ExprF, String]] = List(
    eval0,
    eval1,
    eval2,
    eval3,
    eval4,
    eval5)

  lazy val eval0: Free[ExprF, String] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.roll(ProdF(
          Free.roll(ConstF(2)),
          Free.roll(VarF("x"))
        )),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval1: Free[ExprF, String] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.roll(ProdF(
          Free.roll(ConstF(2)),
          Free.pure("x")
        )),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval2: Free[ExprF, String] =
    Free.roll(QuotF(
      Free.roll(AddF(
        Free.pure("2x"),
        Free.roll(ConstF(1))
      )),
      Free.roll(ConstF(8))
    ))

  lazy val eval3: Free[ExprF, String] =
    Free.roll(QuotF(
      Free.pure("2x + 1"),
      Free.roll(ConstF(8))
    ))

  lazy val eval4: Free[ExprF, String] =
    Free.roll(QuotF(
      Free.pure("2x + 1"),
      Free.pure("8")
    ))

  lazy val eval5: Free[ExprF, String] =
    Free.pure("(2x + 1)/8")
}

object Deutzia {

  def render: IO[Unit] = for {
    _ <- ImgIO.render(attributed, s"deutzia-attributed")
    _ <- steps.traverseWithIndexM((subSteps, i) =>
      ImgIO.render(subSteps, s"deutzia-$i"))
    _ <- ImgIO.render(steps, s"deutzia-all")
  } yield ()

  // super ugly. todo: clean me up
  def attributePara[A](
    algebra: RAlgebra[A, ExprF, A]
  )(implicit ev: Project[ExprF, A]): A => Cofree[ExprF, A] =
    scheme.zoo.para[ExprF, A, Cofree[ExprF, A]](
      RAlgebra(in =>
        Cofree(
          algebra(in.map(xy => (xy._1, xy._2.head))),
          in.map(_._2))))

  lazy val f = attributePara(Algebras.differentiate("x"))

  lazy val attributed: Cofree[ExprF, Expr] =
    f(Aalii.exprB)

  lazy val steps: List[Free[ExprF, Expr]] =
    scheme
      .cata(stratagem.perimeterPartials[ExprF, Expr])
      .apply(attributed)
      .reverse

}
