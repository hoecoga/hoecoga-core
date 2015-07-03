package hoecoga.core.util

import org.scalacheck.Arbitrary

trait ArbitraryUtil {
  def sample[A](implicit a: Arbitrary[A]): A = Stream.continually(a.arbitrary.sample).flatten.head

  class Unique[A](implicit arb: Arbitrary[A]) {
    private[this] var set: Set[A] = Set()

    def unique(): A = synchronized {
      val a = Stream.continually(sample[A]).filter(!set.contains(_)).head
      set = set + a
      a
    }
  }
}

object ArbitraryUtil extends ArbitraryUtil
