package hoecoga.core.value

import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import play.api.libs.json.Json

class ValueSpec extends FunSpec with Checkers {
  import ValueSpecHelper._

  describe("Value") {
    it("either") {
      (1 to max).foreach(a => assert(A.either(a) == Right(A(a))))
      assert(A.either(max + 1).isLeft)
    }

    it("apply") {
      assert(A.apply(max + 1).value == max + 1)
    }

    it("format") {
      check { a: A =>
        Json.toJson(a).as[A] == a
      }
    }
  }

  describe("EnumValue") {
    it("either") {
      B.values.map(_.value).foreach(a => assert(B.either(a) == Right(B(a))))
      assert(B.either("e").isLeft)
    }

    it("apply") {
      intercept[IllegalArgumentException](B.apply("e"))
    }

    it("format") {
      check { b: B =>
        Json.toJson(b).as[B] == b
      }
    }
  }
}

object ValueSpecHelper {
  val max = 100

  case class A(value: Int) extends IntValue

  object A extends IntValueCompanion[A] {
    override protected val pf: PartialFunction[Int, A] = {
      case a if a <= max => A(a)
    }

    override implicit val arbitrary: Arbitrary[A] = Arbitrary(Gen.choose(0, max).map(apply))
  }

  sealed abstract class B(val value: String) extends EnumValue[String]

  object B extends EnumValueCompanion[String, B] {
    case object C extends B("c")
    case object D extends B("d")

    override val values: Set[B] = Set(C, D)
  }
}
