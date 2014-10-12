package grammar.derivation

import org.scalatest.FunSpec
import grammar.{GSym, Grammar}
import util.TestUtils
/**
 * @author A.Sirenko
 *          Date: 9/22/13
 */

class DerivationTest extends FunSpec {
    describe("A Derivation") {

        it("should produce correct chains for english letters") {
            val grammar = Grammar.createMock()
            val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))

            val deriv = Derivation.createForStrings
            val reached = deriv.compute(new Query(sentence, grammar, 5, 5, 3))
            assert(!reached.aggrSyms.keys.isEmpty)
            val reachedLetters = reached.aggrSyms.keys.foldLeft(Set[String]())((s, v) => s + v.name)
            assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
            val reachedSyms = reached.aggrSyms
            for ((sym: GSym, derivDetails: AggrDerivSym) <- reachedSyms) {
                Console.out.println("Symbol " + sym.getKey + ": " + sym.name)
                for (chain <- derivDetails.chains) {
                    Console.out.println("Chain " + sym + ": " + chain)
                }
            }
        }

        it("should return symbols with chains") {
            val grammar = Grammar.createMock()
            val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))

            val deriv = Derivation.createForStrings
            val reached = deriv.compute(new Query(sentence, grammar, 5, 5, 3))
            assert(!reached.aggrSyms.keys.isEmpty)
            val reachedLetters = reached.aggrSyms.keys.foldLeft(Set[String]())((s, v) => s + v.name)
            assert(TestUtils.areSame(reachedLetters, Set("b", "d")))
            val reachedSyms = reached.aggrSyms
            for ((sym: GSym, derivDetails: AggrDerivSym) <- reachedSyms) {
                Console.out.println("Symbol " + sym.getKey + ": " + sym.name)
                for (chain <- derivDetails.chains) {
                    Console.out.println("Chain " + sym + ": " + chain)
                }
            }
        }

        it("should operate on English language") {
            val grammar = Grammar.createEnglishGrammar()
            val deriv = Derivation.createForDictionary(grammar)
            val querySyms = grammar.getSymbols(grammar.lemmatizer.tokenizeAndLemmatize("Russian apple", false))

            val result = deriv.compute(new Query(querySyms, grammar, 10, 25, 5))
            assert(result.aggrSyms.keys.size > 12 && result.aggrSyms.keys.size < 25)

            val names = result.aggrSyms.keys.foldLeft(Set.empty[String])((a,b)=> a + b.name)
            val shouldBe = List(
                    "red", "core", "peach", "food", "worm", "pie", "green", "crisp",
                    "orchard apple tree", "Malus pumila"
            )
            shouldBe.foreach(x => assert(names.contains(x)))
        }

        it("should operate long-running query") {
            val grammar = Grammar.createEnglishGrammar()
            val deriv = Derivation.createForDictionary(grammar)
            val querySyms = grammar.getSymbols(grammar.lemmatizer.tokenizeAndLemmatize("house with girls", false))

            val result = deriv.compute(new Query(querySyms, grammar, 200, 50, 3))
            assert(result.aggrSyms.keys.size > 12)

            val names = result.aggrSyms.keys.foldLeft(Set.empty[String])((a,b)=> a + b.name)
        }
    }
}
