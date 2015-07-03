package hoecoga.core.util

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import play.api.libs.json.{Format, Json}

class JsonUtilSpec extends FunSpec with Checkers {
  describe("JsonUtil") {
    it("format") {
      def test[V](implicit f: Format[V], a: Arbitrary[V]) = {
        check { v: V =>
          Json.toJson(v).validate[V].asOpt === Some(v)
        }
      }

      case class A(a: Boolean)
      case class B(a: A, b: Int)
      case class C(a: A, b: B, c: Long)
      case class D(a: A, b: B, c: C, d: Float)
      case class E(a: A, b: B, c: C, d: D, e: Double)
      case class F(a: A, b: B, c: C, d: D, e: E, f: Short)
      case class G(a: A, b: B, c: C, d: D, e: E, f: F, g: String)

      implicit val fa: Format[A] = JsonUtil.format("a", A.apply, A.unapply)
      implicit val fb: Format[B] = JsonUtil.format("a", "b", B.apply, B.unapply)
      implicit val fc: Format[C] = JsonUtil.format("a", "b", "c", C.apply, C.unapply)
      implicit val fd: Format[D] = JsonUtil.format("a", "b", "c", "d", D.apply, D.unapply)
      implicit val fe: Format[E] = JsonUtil.format("a", "b", "c", "d", "e", E.apply, E.unapply)
      implicit val ff: Format[F] = JsonUtil.format("a", "b", "c", "d", "e", "f", F.apply, F.unapply)
      implicit val fg: Format[G] = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", G.apply, G.unapply)

      implicit val aa = Arbitrary(Gen.resultOf(A.apply _))
      implicit val ab = Arbitrary(Gen.resultOf(B.apply _))
      implicit val ac = Arbitrary(Gen.resultOf(C.apply _))
      implicit val ad = Arbitrary(Gen.resultOf(D.apply _))
      implicit val ae = Arbitrary(Gen.resultOf(E.apply _))
      implicit val af = Arbitrary(Gen.resultOf(F.apply _))
      implicit val ag = Arbitrary(Gen.resultOf(G.apply _))

      test[A]
      test[B]
      test[C]
      test[D]
      test[E]
      test[F]
      test[G]
    }
  }
}
