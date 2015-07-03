package hoecoga.core.util

import org.scalatest.FunSpec

class ArbitraryUtilSpec extends FunSpec with ArbitraryUtil {
  describe("ArbitraryUtil#Unique") {
    it("unique") {
      val unique = new Unique[Int]()
      val list = List.fill(1000)(unique.unique())
      assert(list.toSet.size == list.size)
    }
  }
}
