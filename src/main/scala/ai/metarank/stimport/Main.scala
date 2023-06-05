package ai.metarank.stimport

import ai.metarank.stimport.Sample.PosPair
import cats.effect.{ExitCode, IO, IOApp}
import fs2._
import fs2.io._
import fs2.io.file.{Files, Path}

import java.io.{BufferedInputStream, File, FileInputStream}
import java.util.zip.GZIPInputStream

object Main extends IOApp with Logging {

  override def run(args: List[String]): IO[ExitCode] = {
    args match {
      case path :: Nil =>
        Files[IO]
          .list(Path(path))
          .filter(_.fileName.toString.contains("json"))
          .parEvalMapUnordered(16)(file => decodeOne(file))
          .compile
          .fold(0L)(_ + _)
          .flatTap(total => info(s"total positives: ${total}"))
          .map(_ => ExitCode.Success)
      case _ => IO.raiseError(new Exception("wrong args"))
    }
  }

  def decodeOne(str: Path): IO[Long] =
    readInputStream[IO](
      IO(
        new BufferedInputStream(
          new GZIPInputStream(new FileInputStream(str.toNioPath.toFile)),
          1024000
        )
      ),
      1024 * 1024
    ).handleErrorWith(ex =>
      Stream.eval(error(s"oops: $str", ex)).flatMap(_ => Stream.empty)
    ).through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .filter(_.nonEmpty)
      //.through(PrintProgress.tap("lines"))
      .evalMapChunk(line => IO.fromEither(PosPair.fromString(line)))
      .flatMap(batch => Stream(batch: _*))
      .compile
      .count
      .flatTap(cnt => info(s"${str.fileName}: $cnt"))

}
