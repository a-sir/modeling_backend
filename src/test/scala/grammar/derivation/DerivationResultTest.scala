package grammar.derivation

import grammar.GSym
import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 7/27/14
 */
class DerivationResultTest extends FunSpec {

  describe("An AggrDerivSym") {

    it("should be serialized and parsed from json") {
      val gsym: GSym = new GSym(1, "name1")
      val derivSym = AggrDerivSym(gsym, 0.5, List[String]("a->b", "a->c"))
      val jsObject = derivSym.toJson
      val s2: AggrDerivSym = AggrDerivSym.fromJson(jsObject)
      assert(s2.sym == gsym)
    }

  }

  describe("An DerivResult") {

    it("should be serialized and parsed from json") {

      val gsym1: GSym = new GSym(1, "name1")
      val derivSym1 = AggrDerivSym(gsym1, 0.5, List[String]("a->b", "a->c"))

      val gsym2: GSym = new GSym(2, "name2")
      val derivSym2 = AggrDerivSym(gsym2, 0.5, List[String]("a->b", "a->c"))

      val m: Map[GSym, AggrDerivSym] = Map(gsym1 -> derivSym1, gsym2 -> derivSym2)

      val res: DerivationResult = new DerivationResult(m)
      val jsObject = res.toJson
      val s2: DerivationResult = DerivationResult.fromJson(jsObject)
      assert(s2.aggrSyms.size == res.aggrSyms.size)
    }

  }
}
