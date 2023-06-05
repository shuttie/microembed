package ai.metarank.stimport
import io.circe.{Decoder, Json, JsonObject}
import io.circe.parser._
import io.circe.generic.semiauto._
import io.circe.syntax._

sealed trait Sample {
  def sentence: String
  def pos: String
}

object Sample {
  case class PosPair(sentence: String, pos: String) extends Sample

  object PosPair {
    def fromString(line: String): Either[Throwable, List[PosPair]] = {
      if (line.startsWith("[")) {
        for {
          list <- decode[List[String]](line)
          sample <- list match {
            case a :: b :: Nil      => Right(List(PosPair(a, b)))
            case a :: b :: _ :: Nil => Right(List(PosPair(a, b)))
            case _                  => Left(new Exception(s"cannot parse $line"))
          }
        } yield {
          sample
        }
      } else if (line.startsWith("{")) {
        for {
          json <- parse(line)
          samples <- decodeObject(json).map(_.explodePair)
        } yield {
          samples
        }
      } else {
        Left(new Exception(s"cannot parse $line"))
      }
    }
  }

  def decodeObject(obj: Json): Either[Throwable, JsonFormat] = {
    obj.asObject match {
      case None => Left(new Exception(s"not an object: $obj"))
      case Some(jobj) =>
        if (jobj.contains("set")) {
          JsonFormat.setDecoder.decodeJson(obj)
        } else if (jobj.contains("query")) {
          if (jobj.contains("neg")) {
            JsonFormat.qnDecoder.decodeJson(obj)
          } else {
            JsonFormat.qDecoder.decodeJson(obj)
          }
        } else {
          Left(new Exception(s"wrong format: $obj"))
        }
    }
  }

  case class PosNegPair(sentence: String, pos: String, neg: String)
      extends Sample

  object PosNegPair {
    def fromString(line: String): Either[Throwable, List[PosNegPair]] = {
      if (line.startsWith("[")) {
        for {
          list <- decode[List[String]](line)
          sample <- list match {
            case a :: b :: Nil      => Right(Nil)
            case a :: b :: c :: Nil => Right(List(PosNegPair(a, b, c)))
            case _                  => Left(new Exception(s"cannot parse $line"))
          }
        } yield {
          sample
        }
      } else if (line.startsWith("{")) {
        for {
          json <- parse(line)
          samples <- decodeObject(json).map(_.explodeTriplet)
        } yield {
          samples
        }
      } else {
        Left(new Exception(s"cannot parse $line"))
      }
    }

  }

  sealed trait JsonFormat {
    def explodePair: List[PosPair]
    def explodeTriplet: List[PosNegPair]
  }
  object JsonFormat {
    case class SetFormat(set: List[String]) extends JsonFormat {
      def explodePair: List[PosPair] = for {
        q1 <- set
        q2 <- set if (q1 != q2)
      } yield {
        PosPair(q1, q2)
      }

      override def explodeTriplet: List[PosNegPair] = Nil
    }

    case class QueryFormat(query: String, pos: List[String])
        extends JsonFormat {
      def explodePair: List[PosPair] = pos.map(p => PosPair(query, p))

      override def explodeTriplet: List[PosNegPair] = Nil
    }

    case class QueryNegFormat(
        query: String,
        pos: List[String],
        neg: List[String]
    ) extends JsonFormat {
      override def explodePair: List[PosPair] = pos.map(p => PosPair(query, p))
      def explodeTriplet: List[PosNegPair] = for {
        p <- pos
        n <- neg
      } yield {
        PosNegPair(query, p, n)
      }
    }

    implicit val setDecoder: Decoder[SetFormat] = deriveDecoder
    implicit val qDecoder: Decoder[QueryFormat] = deriveDecoder
    implicit val qnDecoder: Decoder[QueryNegFormat] = deriveDecoder
  }

}
