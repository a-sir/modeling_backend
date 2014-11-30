package grammar

import assoc_net.{Connection, AssociativeNet}
import lemms.{LemmatizerSameImpl, Lemmatizer, LemmatizerImpl}
import synsets.{Loader, Synsets}
import import_ling.CognemReader
import scala.collection.JavaConverters._
import cognems.Cognem

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Grammar(
        val syms: SymbolsMutable,
        val assocRules: RulesMutable,
        val cognRules: CognitiveRulesImmutable,
        val synRules: RulesMutable,
        val lemmatizer: Lemmatizer) {

    override def toString = "Grammar with " + assocRules.rulesCount + " assoc rules"

    def getAssocRulesCount = assocRules.rulesCount
    def getCognRulesCount = cognRules.size
    def getSynRulesCount = synRules.rulesCount
    def countOfSyms = syms.size


    def getSymbols(tokens: List[String]): List[GSym] = {
        tokens.foldRight(List.empty[GSym])((a, b) => {
            val sym = syms.getSymbol(a)
            if (sym.isEmpty) {
                b
            } else {
                sym.get :: b
            }
        })
    }
}

object Grammar {

    def createEnglishGrammar(): Grammar = {
        create(AssociativeNet.loadDefaultNet(), LemmatizerImpl.create(), CognemReader.defaultSet)
    }
    

    def create(assoc: AssociativeNet, lemmatizer: LemmatizerImpl, cognems: List[Cognem]): Grammar = {
        val syms = new SymbolsMutable
        val rules = new RulesMutable

        val keepUnknownWordforms = true
        val skipStopWords = true;
        // Assume assoc
        for (stim: String <- assoc.getStims.asScala.toIterable) {
            var total = 0
            val conns: Iterable[Connection] = assoc.getConnsForStim(stim).getConns.asScala.toIterable
            for (conn: Connection <- conns) {
                total += conn.getCount
            }

            val lemStimSymbols = syms.getOrCreateSymbols(
                lemmatizer.tokenizeAndLemmatize(stim, keepUnknownWordforms, skipStopWords)
            )
            if (!lemStimSymbols.isEmpty) {
                for (conn: Connection <- conns) {
                    val lemReakSymbols = syms.getOrCreateSymbols(
                        lemmatizer.tokenizeAndLemmatize(conn.getReak, keepUnknownWordforms, skipStopWords)
                    )
                    if (!lemReakSymbols.isEmpty) {
                        val count = conn.getCount
                        val rule = Rule.createAssoc(lemStimSymbols, lemReakSymbols, count * 1.0 / total)
                        rules.addRule(rule)
                    }
                }
            }
        }

        val cognRules = new CognitiveRulesMutable
        for (cogn <- CognemReader.filterCognemsByChars(cognems)) {
            val left = syms.getOrCreateSymbols(lemmatizer.tokenizeAndLemmatize(cogn.name, keepUnknownWordforms, skipStopWords))
            val right = syms.getOrCreateSymbols(lemmatizer.tokenizeAndLemmatize(cogn.sense, keepUnknownWordforms, skipStopWords))
            if (!left.isEmpty && !right.isEmpty) {
                cognRules.addRule(new CognitiveRule(left, right, util.Collections.toList(cogn.context)))
            }
        }

        // synsets
        val rawSyns: Synsets = Loader.readDefault()
        val synRules = new RulesMutable
        for (i <- 0 to (rawSyns.size() - 1)) {
            val synsetSymbols = util.Collections.toList(rawSyns.getSynset(i))
                    .foldLeft(Set[GSym]())((x, y) => x + syms.getOrCreateSymbol(y))
            for (s1 <- synsetSymbols) {
                for (s2 <- synsetSymbols.filter(_ != s1)) {
                    synRules.addRule(Rule.createSyn(List(s1), List(s2)))
                }
            }
        }

        new Grammar(syms, rules, cognRules.immutableInstance, synRules, lemmatizer)
    }

    def createMock(): Grammar = {
        val syms = new SymbolsMutable

        val assocRules = new RulesMutable
        assocRules.addRule(
            Rule.createAssoc(
                syms.getOrCreateSymbols(List("a")),
                syms.getOrCreateSymbols(List("b")), 0.5))
        assocRules.addRule(
            Rule.createAssoc(
                syms.getOrCreateSymbols(List("b")),
                syms.getOrCreateSymbols(List("a", "b")), 0.3))

        val cognRules = new CognitiveRulesMutable
        cognRules.addRule(
            new CognitiveRule(
                syms.getOrCreateSymbols(List("a", "c")),
                syms.getOrCreateSymbols(List("d")),
                List("alphabet"))
        )

        val synRules = new RulesMutable
        synRules.addRule(
            Rule.createSyn(
                syms.getOrCreateSymbols(List("c")),
                syms.getOrCreateSymbols(List("d")))
        )

        new Grammar(syms, assocRules, cognRules.immutableInstance, synRules, new LemmatizerSameImpl())
    }

}
