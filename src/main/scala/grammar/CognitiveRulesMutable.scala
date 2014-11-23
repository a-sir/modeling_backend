package grammar

import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRulesMutable {

	private var contexts: Set[String] = Set.empty
	private var rules: List[Rule] = List()
	private val rulesByContext
			= new mutable.HashMap[String, mutable.Set[Rule]] with mutable.MultiMap[String, Rule]
	private val rulesBySign
			= new mutable.HashMap[GSym, mutable.Set[Rule]] with mutable.MultiMap[GSym, Rule]
	private val rulesBySense
			= new mutable.HashMap[GSym, mutable.Set[Rule]] with mutable.MultiMap[GSym, Rule]

	def getByContext(context: String) = rulesByContext.get(context)

	def getBySign(sym: GSym) = rulesBySign.get(sym)

	def getBySense(sym: GSym) = rulesBySense.get(sym)

	def addRule(rule: CognitiveRule) {
	    val r = Rule.createCognitive(rule.left, rule.right)
	    contexts = contexts ++ rule.context
		for (ctx <- rule.context) {
			rulesByContext.addBinding(ctx, r)
		}
		for (leftSym <- rule.left) {
			rulesBySense.addBinding(leftSym, r)
		}
		for (rightSym <- rule.right) {
			rulesBySign.addBinding(rightSym, r)
		}
		rules = r :: rules
	}

	def immutableInstance = new CognitiveRulesImmutable(
        contexts, rulesByContext, rulesBySign, rulesBySense, rules
    )
}