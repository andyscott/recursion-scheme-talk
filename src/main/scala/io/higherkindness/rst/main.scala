package io.higherkindness.rst

import cats._
import cats.effect._
import cats.implicits._

import reftree.core._
import reftree.render._
import reftree.diagram._
import reftree.geometry.Color
import java.nio.file.Paths

object ImgIO {
  lazy val renderer = Renderer(
    renderingOptions = RenderingOptions(
      density = 75,
      font = "Atlas Grotesk",
      palette = IndexedSeq(Color.fromRgbaString("#1B2733"))
    ),
    directory = Paths.get("out"))
  import renderer._

  def render[A: Show: ToRefTree](a: A, name: String): IO[Unit] =
    IO(Diagram(a).withCaption(a.show).render(name))
}

object StdIO {
  def println(msg: String): IO[Unit] = IO(Console.println(msg))
}

object Main {

  def main(args: Array[String]): Unit =
    main(args.toList).unsafeRunSync

  implicit val bigDecimalToRefTree: ToRefTree[BigDecimal] =
    ToRefTree(v =>
      RefTree.Val(v.toInt)) // ignore overflow for now

  import reftree.contrib.SimplifiedInstances._

  def main(args: List[String]): IO[Unit] =
    for {
      _ <- StdIO.println("aalii")
      _ <- ImgIO.render(Aalii.expr, "aalii-expr")
    } yield ()

}

object Aalii {
  import Expr._

  // 2x + 1
  val expr: Expr = Add(Prod(Const(2), Var("x")), Const(1))
}
