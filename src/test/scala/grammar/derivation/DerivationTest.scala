package grammar.derivation

import org.scalatest.FunSpec
import grammar.Grammar
import util.TestUtils

/**
 * @author A.Sirenko
 *          Date: 9/22/13
 */
class DerivationTest extends FunSpec {
	describe("A Derivation") {

		it("should produce sentences until limits are met") {
			val grammar = Grammar.createMock()
			val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))
			val deriv = new Derivation
			val reached = deriv.compute(new Query(sentence, grammar, 5, 3))
			assert(!reached.symbols.isEmpty)
			val reachedLetters = reached.symbols.foldLeft(Set[String]())((s, v) => s + v.name)
			assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
		}
	}
}