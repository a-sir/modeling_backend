package grammar.derivation

import org.scalatest.FunSpec
import grammar.Grammar
import util.TestUtils
/**
 * @author A.Sirenko
 *          Date: 9/22/13
 */

object DerivationTest {

  def main(args: Array[String]): Unit = {
            val grammar = Grammar.createMock()
            val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))

            val deriv = Derivation.creaqteForStrings
            val reached = deriv.compute(new Query(sentence, grammar, 5, 3))
            assert(!reached.symbols.isEmpty)
            val reachedLetters = reached.symbols.foldLeft(Set[String]())((s, v) => s + v.name)
            assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
            val reachedSyms = reached.aggrSyms
            for (r <- reachedSyms) {
              Console.out.println("Symbol " + r._1 + ": " + r._2._1)
              for (chain <- r._2._2) {
                Console.out.println("Chain " + r._1 + ": " + chain)
              }
            }
  }

}

class DerivationTest extends FunSpec {
	describe("A Derivation") {

	it("should produce sentences until limits are met") {
		val grammar = Grammar.createMock()
		val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))

		val deriv = Derivation.createForStrings
		val reached = deriv.compute(new Query(sentence, grammar, 5, 3))
		assert(!reached.symbols.isEmpty)
		val reachedLetters = reached.symbols.foldLeft(Set[String]())((s, v) => s + v.name)
		assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
	}

          it("should return symbols with chains") {
            val grammar = Grammar.createMock()
            val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))

            val deriv = Derivation.createForStrings
            val reached = deriv.compute(new Query(sentence, grammar, 5, 3))
            assert(!reached.symbols.isEmpty)
            val reachedLetters = reached.symbols.foldLeft(Set[String]())((s, v) => s + v.name)
            assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
            val reachedSyms = reached.aggrSyms
            for (r <- reachedSyms) {
              Console.out.println("Symbol " + r._1 + ": " + r._2._1)
              for (chain <- r._2._2) {
                Console.out.println("Chain " + r._1 + ": " + chain)
              }
            }
          }

        it("should operate on English language") {
            val grammar = Grammar.createEnglishGrammar()
            val deriv = Derivation.createForDictionary(grammar)
            val querySyms = grammar.getSymbols(grammar.lemmatizer.tokenizeAndLemmatize("Russian apple", false))

            val result = deriv.compute(new Query(querySyms, grammar, 10, 5))
            assert(result.symbols.size > 12 && result.symbols.size < 25)

            val names = result.symbols.foldLeft(Set.empty[String])((a,b)=> a + b.name)
            val shouldBe = List(
                    "red", "core", "peach", "food", "worm", "pie", "green", "crisp",
                    "orchard apple tree", "Malus pumila"
            )
            shouldBe.foreach(x => assert(names.contains(x)))
        }
	}
}
