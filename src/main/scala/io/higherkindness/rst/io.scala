package io.higherkindness.rst

import cats.effect._
import cats.implicits._

import reftree.core._
import reftree.render._
import reftree.diagram._
import reftree.geometry.Color
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

object ImgIO {
  lazy val outputPath: Path = Paths.get("out")

  private lazy val renderer = Renderer(
    renderingOptions = RenderingOptions(
      density = 75,
      font = "Atlas Grotesk",
      palette = IndexedSeq(Color.fromRgbaString("#1B2733"))
    ),
    directory = outputPath)
  import renderer._

  def render[A: ToRefTree](a: A, name: String): IO[Unit] =
    StdIO.println(s" + rendering diagram $name") *>
    IO(Diagram(a).render(name))

  def render[A: ToRefTree](a: List[A], name: String): IO[Unit] =
    StdIO.println(s" + rendering animation $name") *>
    IO(Animation(a.map(Diagram(_))).render(name))
}

object StdIO {
  def println(msg: String): IO[Unit] = IO(Console.println(msg))
}

object FileIO {

  def deleteRecursively(file: File): IO[Unit] =
    if (file.isDirectory)
      file.listFiles.toList.traverse_(deleteRecursively)
    else if (file.exists)
      IO(file.delete).flatMap(deleted =>
        if (deleted) IO.pure(())
        else IO.raiseError(new Exception(s"Unable to delete ${file.getAbsolutePath}")))
    else
      IO.pure(())
}
