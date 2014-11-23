package grammar

import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRulesImmutable(
		val contexts: Set[String],
		val rulesByContext: mutable.MultiMap[String, Rule],
		val rulesBySign: mutable.MultiMap[GSym, Rule],
		val rulesBySense: mutable.MultiMap[GSym, Rule],
		val allRules: List[Rule]) {
	def size = allRules.size
	
	def getPossibleRulesFor(sentence: List[GSym]): Option[Set[Rule]] = {
        var set: Set[Rule] = Set.empty
        for (s <- sentence) {
            rulesBySense.get(s) match {
                case Some(x) => for (xv <- x) { set += xv }
                case None => println ("Nothing for " + s)
            }
        }
        if (set.isEmpty) { 
            None
        } else {
            Some(set)
        }
	}
}
