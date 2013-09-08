package lemms

import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class LemmatizerTest extends FunSpec {

	lazy val lemmatizer: Lemmatizer = Lemmatizer.create()

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

	}

}
