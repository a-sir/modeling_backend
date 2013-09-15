package grammar

import org.scalatest.FunSpec
import assoc_net.AssociativeNet
import lemms.Lemmatizer

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class GrammarTest extends FunSpec {
	describe("A Grammar") {

		it("should consist of N rules") {
			val assocNet = AssociativeNet.loadDefaultNet()
			val g = Grammar.create(assocNet, Lemmatizer.create())
		}

	}

}
