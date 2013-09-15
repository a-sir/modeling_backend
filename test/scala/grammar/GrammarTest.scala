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

		it("should consist of associative, cognitive rules and synonims") {
			val assocNet = AssociativeNet.loadDefaultNet()
			val g: Grammar = Grammar.create(assocNet, Lemmatizer.create())
			assert(g.getAssocRulesCount == 69405)
			assert(g.getCognRulesCount == 740069)
			assert(g.getSynsetsCount == 206978)
		}

	}

}
