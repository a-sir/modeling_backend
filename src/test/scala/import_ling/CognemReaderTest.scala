package import_ling

import org.scalatest.FunSpec
import lemms.LemmatizerImpl
import cognems.Cognem

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognemReaderTest extends FunSpec {
	describe("A CognemReader") {
		ignore("default data-set should contain 1120k cognems before language filtering and 740k after") {
			val allCognems = CognemReader.defaultSet
			assert(allCognems.length == 1117697)
			assert(CognemReader.filterCognemsByChars(allCognems).length == 485079)
		}
	}
}
