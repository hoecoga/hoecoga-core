package hoecoga.core

import hoecoga.core.enum.{WeekInt, WeekString}
import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import play.api.libs.json.{Format, Json}

class EnumCodeGeneratorSpec extends FunSpec with Checkers {
  describe("EnumCodeGenerator") {
    it("code") {
      def test[A, B <: EnumValue[A]](either: A => Either[IllegalArgumentException, B])
                                    (implicit arbitrary: Arbitrary[B], format: Format[B]) = {
        check { b: B =>
          either(b.value) === Right(b)
        }
        check { b: B =>
          Json.toJson(b).as[B] === b
        }
      }

      test[Int, WeekInt](WeekInt.either)
      test[String, WeekString](WeekString.either)
    }
  }
}
