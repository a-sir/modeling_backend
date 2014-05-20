package lemms

import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class LemmatizerImplTest extends FunSpec {

	lazy val lemmatizer: LemmatizerImpl = LemmatizerImpl.create()

	describe("A Lemmatizer") {

		it("should correctly respond on not-existed terms") {
			val notExist = "asdasd"
			assert(lemmatizer.getLemmas(notExist) == Option.empty)
			assert(lemmatizer.getWordforms(notExist) == Option.empty)
		}

		it("should get lem and wordforms for apple") {
			assert(lemmatizer.getLemmas("apples") == Option(List("apple")))
			printf("%d", lemmatizer.getWordforms("apple").get.length)
			assert(lemmatizer.getWordforms("apple").get == List("apples", "apple"))
		}

		it("should consider several delimeters") {
			assert(LemmatizerImpl.tokenize("abra cadabra,'really':fun") == List("abra", "cadabra", "really", "fun"))
		}

		it("should be able to prepare lemmatized tokens") {
			var keepUnknown = true
			assert(lemmatizer.tokenizeAndLemmatize("apples,; cadabra", keepUnknown) == List("apple", "cadabra"))
			keepUnknown = false
			assert(lemmatizer.tokenizeAndLemmatize("apples,; cadabra", keepUnknown) == List("apple"))
		}

	}

}
