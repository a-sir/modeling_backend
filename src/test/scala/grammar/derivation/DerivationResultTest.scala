package grammar.derivation

import grammar.GSym
import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 7/27/14
 */
class DerivationResultTest extends FunSpec {
	describe("A DerivationResult") {

    it("should be serialized and parsed from json") {
      val gsym: GSym = new GSym(1, "name1")
      val derivSym = AggrDerivSym(gsym, 0.5, List[String]("a->b", "a->c"))
      val jsObject = derivSym.toJson
      val s2: AggrDerivSym = AggrDerivSym.fromJson(jsObject)
      assert(s2.sym == gsym)
    }

  }
}
