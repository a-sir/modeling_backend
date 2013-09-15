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
			= new mutable.HashMap[Symbol, mutable.Set[CognitiveRule]] with mutable.MultiMap[Symbol, CognitiveRule]
	private val rulesBySense
			= new mutable.HashMap[Symbol, mutable.Set[CognitiveRule]] with mutable.MultiMap[Symbol, CognitiveRule]

	def getByContext(context: String) = rulesByContext.get(context)

	def getBySign(sym: Symbol) = rulesBySign.get(sym)

	def getBySense(sym: Symbol) = rulesBySense.get(sym)

	def addRule(rule: CognitiveRule) {
		contexts = contexts ++ rule.context
		for (ctx <- rule.context) {
			rulesByContext.addBinding(ctx, rule)
		}
		for (leftSym <- rule.left) {
			rulesBySign.addBinding(leftSym, rule)
		}
		for (rightSym <- rule.right) {
			rulesBySense.addBinding(rightSym, rule)
		}
		rules = rule :: rules
	}

	def immutableInstance = new CognitiveRulesImmutable(contexts, rulesByContext, rulesBySign, rulesBySense, rules)
}