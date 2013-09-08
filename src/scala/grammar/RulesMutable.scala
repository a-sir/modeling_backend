package grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class RulesMutable {

	var rules: Set[Rule] = Set.empty

	var ruleByLeft: Map[Symbol, List[Rule]] = Map.empty

	var ruleByRight: Map[Symbol, List[Rule]] = Map.empty

	def addRule(rule: Rule) {
		rules = rules + rule
		ruleByLeft = RulesMutable.addRuleForSymbols(rule, rule.left, ruleByLeft)
		ruleByRight = RulesMutable.addRuleForSymbols(rule, rule.right, ruleByRight)
	}

	def getByLeft(sym: Symbol) = ruleByLeft.get(sym)

	def getByRight(sym: Symbol) = ruleByRight.get(sym)

	def rulesCount = rules.size
}

object RulesMutable {

	def addRuleForSymbols(rule: Rule, syms: List[Symbol], map: Map[Symbol, List[Rule]]): Map[Symbol, List[Rule]] = {
		var res = map
		for (sym <- syms.distinct) {
			val rules = map.get(sym)
			if (rules == Option.empty || rules.get.length == 0) {
				res = res + (sym -> List(rule))
			} else {
				res = res + (sym -> (rules.get ::: List(rule)).distinct)
			}
		}
		res
	}
}
