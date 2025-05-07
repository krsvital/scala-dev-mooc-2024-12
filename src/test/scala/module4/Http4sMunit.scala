import cats.effect._
import org.http4s._
import ru.otus.module4.http4s.http4s_homework._
import io.circe.generic.auto._, io.circe.syntax._, io.circe.Encoder, io.circe.Decoder, io.circe.parser.parse
import io.circe.generic.semiauto.deriveEncoder
import io.circe.Json
import java.util.Calendar

class MyHttpRoutesSuite extends munit.Http4sSuite {
    val ref = Ref.of[IO, Int](0).unsafeRunSync()

    case class Count(counter: Int)
    implicit val encoder: Encoder[Count] = deriveEncoder[Count]

    override def http4sMUnitClientFixture = homeworkService(ref).orFail.asFixture
  
    test(GET(uri"counter")).alias("get counter 1") { response =>
        assertIO(response.as[String], Count(1).asJson.toString())
    }

    test(GET(uri"counter")).alias("get counter 2") { response =>
        assertIO(response.as[String], Count(2).asJson.toString())
    }

    test(GET(uri"slow/3/10/1")).alias("slow chunks size") { response =>
        assertIO(response.as[String].map{ s => s.size }, 10)
    }

    test(GET(uri"slow/3/10/1")).alias("slow chunks time") { response => {
            val time = getTime
            assertIO(response.as[String].map{ s => getTime - time  }, 3)
        }
    }

    def getTime: Int = (Calendar.getInstance().getTimeInMillis()/1000).toInt
}