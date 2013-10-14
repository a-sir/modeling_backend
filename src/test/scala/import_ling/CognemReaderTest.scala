package import_ling

import org.scalatest.FunSpec
import lemms.Lemmatizer
import cognems.Cognem

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognemReaderTest extends FunSpec {

	describe("A CognemReader") {

		it("should parse correct sequence of lines") {
			val lines = List("# dictionary", "---|A reference work", "hurling,lang=en|The usual")
			val cognems: List[Cognem] = CognemReader.parse(lines)
			assert(cognems.length == 2)
			val c1 = cognems(1)
			val c2 = cognems(0)
			assert(c1.name == "dictionary" && c1.sense == "A reference work" && c1.context.isEmpty)
			assert(
				c2.name == "dictionary"
						&& c2.sense == "The usual"
			)
			assert(util.Collections.toList(c2.context) === List("hurling", "lang=en"))
		}

		it("should throw exception if not enough data to parse") {
			intercept[RuntimeException] {
				CognemReader.parse(List("# name"))
			}
			intercept[RuntimeException] {
				CognemReader.parse(List("---|area"))
			}
		}

		it("default data-set should contain 1120k cognems before language filtering and 740k after") {
			val allCognems = CognemReader.defaultSet
			assert(allCognems.length == 1117697)
			assert(CognemReader.filterCognemsByChars(allCognems).length == 740069)
		}

	}
}
