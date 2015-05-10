package hoecoga.core

import java.util.Date

import hoecoga.core.JsonUtilSpecHelper._
import org.scalacheck.Arbitrary
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

      implicit val fa = JsonUtil.format("a", A.apply, A.unapply)
      implicit val fb = JsonUtil.format("a", "b", B.apply, B.unapply)
      implicit val fc = JsonUtil.format("a", "b", "c", C.apply, C.unapply)
      implicit val fd = JsonUtil.format("a", "b", "c", "d", D.apply, D.unapply)
      implicit val fe = JsonUtil.format("a", "b", "c", "d", "e", E.apply, E.unapply)
      implicit val ff = JsonUtil.format("a", "b", "c", "d", "e", "f", F.apply, F.unapply)
      implicit val fg = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", G.apply, G.unapply)
      implicit val fh = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", "h", H.apply, H.unapply)
      implicit val fi = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", "h", "i", I.apply, I.unapply)
      implicit val fj = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", J.apply, J.unapply)
      implicit val fk = JsonUtil.format("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", K.apply, K.unapply)

      implicit val aa = ArbitraryUtil.arbitrary(A.apply _)
      implicit val ab = ArbitraryUtil.arbitrary(B.apply _)
      implicit val ac = ArbitraryUtil.arbitrary(C.apply _)
      implicit val ad = ArbitraryUtil.arbitrary(D.apply _)
      implicit val ae = ArbitraryUtil.arbitrary(E.apply _)
      implicit val af = ArbitraryUtil.arbitrary(F.apply _)
      implicit val ag = ArbitraryUtil.arbitrary(G.apply _)
      implicit val ah = ArbitraryUtil.arbitrary(H.apply _)
      implicit val ai = ArbitraryUtil.arbitrary(I.apply _)
      implicit val aj = ArbitraryUtil.arbitrary(J.apply _)
      implicit val ak = ArbitraryUtil.arbitrary(K.apply _)

      test[A]
      test[B]
      test[C]
      test[D]
      test[E]
      test[F]
      test[G]
      test[H]
      test[I]
      test[J]
      test[K]
    }
  }
}

object JsonUtilSpecHelper {
  case class A(a: Boolean)
  case class B(a: A, b: Int)
  case class C(a: A, b: B, c: Long)
  case class D(a: A, b: B, c: C, d: Float)
  case class E(a: A, b: B, c: C, d: D, e: Double)
  case class F(a: A, b: B, c: C, d: D, e: E, f: Short)
  case class G(a: A, b: B, c: C, d: D, e: E, f: F, g: String)
  case class H(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: Date)
  case class I(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: BigDecimal)
  case class J(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: Option[A])
  case class K(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: List[B])
}
