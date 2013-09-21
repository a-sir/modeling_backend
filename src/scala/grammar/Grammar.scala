package grammar

import assoc_net.{Connection, AssociativeNet}
import lemms.Lemmatizer
import synsets.{Loader, Synsets}
import java.nio.file.Paths
import import_ling.CognemReader

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Grammar(
		val syms: SymbolsMutable,
		val assocRules: RulesMutable,
		val cognRules: CognitiveRulesImmutable,
		val synRules: RulesMutable) {

	def getSymbol(name: String) = syms.getSymbol(name)

	def getAssocRulesByLeft(left: Symbol) = assocRules.getByLeft(left)

	def getAssocRulesByRight(right: Symbol) = assocRules.getByRight(right)

	def getCognRulesByLeft(left: Symbol) = cognRules.rulesBySign.get(left)

	def getCognRulesByRight(right: Symbol) = cognRules.rulesBySense.get(right)

	override def toString = "Grammar with " + assocRules.rulesCount + " rules"

	def getAssocRulesCount = assocRules.rulesCount
	def getCognRulesCount = cognRules.size
	def getSynRulesCount = synRules.rulesCount
	def countOfSyms = syms.size
}

object Grammar {

	def create(assoc: AssociativeNet, lemmatizer: Lemmatizer): Grammar = {
		val syms = new SymbolsMutable
		val rules = new RulesMutable

		val keepUnknownWordforms = true

		// Assume assoc
		for (stim: String <- util.Collections.toList(assoc.getStims)) {
			var total = 0
			val conns: List[Connection] = util.Collections.toList(assoc.getConnsForStim(stim).getConns)
			for (conn: Connection <- conns) {
				total += conn.getCount
			}

			val lemStimSymbols = syms.getOrCreateSymbols(
				lemmatizer.tokenizeAndLemmatize(stim, keepUnknownWordforms)
			)
			if (!lemStimSymbols.isEmpty) {
				for (conn: Connection <- conns) {
					val lemReakSymbols = syms.getOrCreateSymbols(
						lemmatizer.tokenizeAndLemmatize(conn.getReak, keepUnknownWordforms)
					)
					if (!lemReakSymbols.isEmpty) {
						val count = conn.getCount
						val rule = new Rule(lemStimSymbols, lemReakSymbols, count * 1.0 / total)
						rules.addRule(rule)
					}
				}
			}
		}

		val cognRules = new CognitiveRulesMutable
		for (cogn <- CognemReader.filterCognemsByChars(CognemReader.defaultSet)) {
			val left = syms.getOrCreateSymbols(lemmatizer.tokenizeAndLemmatize(cogn.name, keepUnknownWordforms))
			val right = syms.getOrCreateSymbols(lemmatizer.tokenizeAndLemmatize(cogn.sense, keepUnknownWordforms))
			cognRules.addRule(new CognitiveRule(left, right, util.Collections.toList(cogn.context)))
		}

		// synsets
		val rawSyns: Synsets = Loader.read(Paths.get("data/princeton_wp_synsets"))
		val synRules = new RulesMutable
		for (i <- 0 to (rawSyns.size() - 1)) {
			val synsetSymbols = util.Collections.toList(rawSyns.getSynset(i))
					.foldLeft(Set[Symbol]())((x, y) => x + syms.getOrCreateSymbol(y))
			for (s1 <- synsetSymbols) {
				for (s2 <- synsetSymbols.filter(_ != s1)) {
					synRules.addRule(new Rule(List(s1), List(s2), 0.0))
				}
			}
		}

		new Grammar(syms, rules, cognRules.immutableInstance, synRules)
	}

}
