package grammar

import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRulesMutable {

	private var contexts: Set[String] = Set.empty
	private var rules: List[CognitiveRule] = List()
	private val rulesByContext
			= new mutable.HashMap[String, mutable.Set[CognitiveRule]] with mutable.MultiMap[String, CognitiveRule]
	private val rulesBySign
			= new mutable.HashMap[GSym, mutable.Set[CognitiveRule]] with mutable.MultiMap[GSym, CognitiveRule]
	private val rulesBySense
			= new mutable.HashMap[GSym, mutable.Set[CognitiveRule]] with mutable.MultiMap[GSym, CognitiveRule]

	def getByContext(context: String) = rulesByContext.get(context)

	def getBySign(sym: GSym) = rulesBySign.get(sym)

	def getBySense(sym: GSym) = rulesBySense.get(sym)

	def addRule(rule: CognitiveRule) {
		contexts = contexts ++ rule.context
		for (ctx <- rule.context) {
			rulesByContext.addBinding(ctx, rule)
		}
		for (leftSym <- rule.left) {
			rulesBySense.addBinding(leftSym, rule)
		}
		for (rightSym <- rule.right) {
			rulesBySign.addBinding(rightSym, rule)
		}
		rules = rule :: rules
	}

	def immutableInstance = new CognitiveRulesImmutable(contexts, rulesByContext, rulesBySign, rulesBySense, rules)
}