package grammar

import org.scalatest.FunSpec
import assoc_net.AssociativeNet
import lemms.LemmatizerImpl
import import_ling.CognemReader
import cognems.Cognem

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class GrammarTest extends FunSpec {
    describe("A Grammar") {
        it("should consist of associative, cognitive rules and synonims") {
            val assocNet = AssociativeNet.loadDefaultNet()
            val cognems: List[Cognem] = CognemReader.defaultSet
            val g: Grammar = Grammar.create(assocNet, LemmatizerImpl.create(), cognems)
            assert(g.getAssocRulesCount === 68338)
            assert(g.getCognRulesCount === 481331)
            assert(g.getSynRulesCount === 315984)
        }
    }
}
