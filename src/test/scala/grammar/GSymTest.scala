package grammar

import org.scalatest.FunSpec

/**
 * author: A.Sirenko
 *          Date: 8/6/14
 */
class GSymTest extends FunSpec {
  describe("A Gsym") {

    it("should be serialized and parsed from json") {
      val gsym: GSym = new GSym(1, "name1")
      val obj = gsym.toJson
      val parsed = GSym.fromJson(obj)
      assert(gsym == parsed)
    }

  }
}
