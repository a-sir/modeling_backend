package grammar

import assoc_net.{Connection, AssociativeNet}
import lemms.{Lemmatizer}

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Grammar(
		val syms: SymbolsMutable, val rules: RulesMutable) {

	def getSymbol(name: String) = syms.getSymbol(name)

	def getRulesByLeft(left: Symbol): Option[List[Rule]] = rules.getByLeft(left)

	def getRulesByRight(right: Symbol): Option[List[Rule]] = rules.getByRight(right)

	override def toString = "Grammar with " + rules.rulesCount + " rules"

	def getRulesCount = rules.rulesCount
}

object Grammar {

	def create(assoc: AssociativeNet, lemmatizer: Lemmatizer): Grammar = {
		val syms = new SymbolsMutable
		val rules = new RulesMutable

		// Assume assoc
		for (stim: String <- util.Collections.toList(assoc.getStims)) {
			var total = 0
			val conns: List[Connection] = util.Collections.toList(assoc.getConnsForStim(stim).getConns)
			for (conn: Connection <- conns) {
				total += conn.getCount
			}
			val keepUnknownTokens = true
			val lemStimSymbols = syms.getOrCreateSymbols(
				lemmatizer.tokenizeAndLemmatize(stim, keepUnknownTokens)
			)
			if (!lemStimSymbols.isEmpty) {
				for (conn: Connection <- conns) {
					val lemReakSymbols = syms.getOrCreateSymbols(
						lemmatizer.tokenizeAndLemmatize(conn.getReak, keepUnknownTokens)
					)
					if (!lemReakSymbols.isEmpty) {
						val count = conn.getCount
						val rule = new Rule(lemStimSymbols, lemReakSymbols, count * 1.0 / total)
						rules.addRule(rule)
					}
				}
			}
		}

		new Grammar(syms, rules)
	}
}
