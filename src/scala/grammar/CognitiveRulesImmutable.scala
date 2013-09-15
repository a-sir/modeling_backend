package grammar

import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRulesImmutable(
		val contexts: Set[String],
		val rulesByContext: mutable.MultiMap[String, CognitiveRule],
		val rulesBySign: mutable.MultiMap[Symbol, CognitiveRule],
		val rulesBySense: mutable.MultiMap[Symbol, CognitiveRule],
		val allRules: List[CognitiveRule]) {
	def size = allRules.size
}
