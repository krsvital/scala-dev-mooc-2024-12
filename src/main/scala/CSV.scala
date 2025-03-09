import scala.deriving.Mirror
import scala.compiletime._
import scala.language.implicitConversions

object CSV {
    trait Decoder[A, B] extends (A => B)
    object Decoder:
      given Decoder[String, String] = s => s
      given Decoder[String, Int] = _.toInt
      given Decoder[String, Long] = _.toLong
      given Decoder[String, Double] = _.toDouble
      given Decoder[String, Boolean] = s => s match
        case "true" => true
        case "false" => false
        case s => throw IllegalArgumentException(s)

      given Decoder[List[String], EmptyTuple] =
        case Nil => EmptyTuple
        case s => throw IllegalArgumentException(s.toString())

      given [H, T <: Tuple](using dh: Decoder[String, H], dt: Decoder[List[String], T]): Decoder[List[String], H *: T] =
        case h::t => dh(h) *: dt(t)
        case Nil => throw IndexOutOfBoundsException()

    def apply[A](string: String)(using delim: Delimiter, m: Mirror.ProductOf[A], decoder: Decoder[List[String], m.MirroredElemTypes]): List[A] =
        string
            .split("\n")
            .toList
            .filterNot(s => s.matches("""\s*"""))
            .map{ s =>
                m.fromProduct(
                    decoder(s.split(delim).toList.map { s =>
                        s.replaceAll("""^\s*""", "").replaceAll("""\s*$""","")
                    }
                )
            )
        }

    opaque type Delimiter = String

    object Delimiter {
      def apply(string: String): Delimiter = string

      given Conversion[Delimiter, String] with
        def apply(x: Delimiter): String = x
    }
}