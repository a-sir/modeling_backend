package grammar

import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRulesImmutable(
		val contexts: Set[String],
		val rulesByContext: mutable.MultiMap[String, CognitiveRule],
		val rulesBySign: mutable.MultiMap[GSym, CognitiveRule],
		val rulesBySense: mutable.MultiMap[GSym, CognitiveRule],
		val allRules: List[CognitiveRule]) {
	def size = allRules.size
}
