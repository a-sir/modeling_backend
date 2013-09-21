package grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class RulesMutable {

	var rules: Set[Rule] = Set.empty

	var ruleByLeft: Map[Symbol, Set[Rule]] = Map.empty

	var ruleByRight: Map[Symbol, Set[Rule]] = Map.empty

	def addRule(rule: Rule) {
		if (!rules.contains(rule)) {
			rules = rules + rule
			ruleByLeft = RulesMutable.addRuleForSymbols(rule, rule.left, ruleByLeft)
			ruleByRight = RulesMutable.addRuleForSymbols(rule, rule.right, ruleByRight)
		}
	}

	def getByLeft(sym: Symbol) = ruleByLeft.get(sym)

	def getByRight(sym: Symbol) = ruleByRight.get(sym)

	def rulesCount = rules.size
}

object RulesMutable {

	def addRuleForSymbols(rule: Rule, syms: List[Symbol], map: Map[Symbol, Set[Rule]]): Map[Symbol, Set[Rule]] = {
		var res = map
		for (sym <- syms.distinct) {
			val rules = map.get(sym)
			if (rules == Option.empty || rules.get.size == 0) {
				res = res + (sym -> Set(rule))
			} else {
				res = res + (sym -> (rules.get + rule))
			}
		}
		res
	}
}
