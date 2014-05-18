package grammar

import utils.Keyable

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Rule(val key: Int, val left: List[GSym], val right: List[GSym], val cost: Double) extends Keyable[Integer] {

	override def toString = "left: " + left + ", right: " + right + "cost: " + cost

    override def getKey: Integer = key

    lazy val leftKeys: List[Integer] = left.foldRight(List.empty[Integer]) ((a, b) => a.key :: b)
}

object Rule {
    var currentKey = 1

    def nextKey: Int = {
        currentKey += 1
        currentKey
    }

    def createCognitive(left: List[GSym], right: List[GSym]): Rule = {
        new Rule(Rule.nextKey, left, right, -1)
    }

    def createWeightedCognitive(left: List[GSym], right: List[GSym], cost: Double): Rule = {
        new Rule(Rule.nextKey, left, right, cost)
    }

    def createSyn(left: List[GSym], right: List[GSym]): Rule = {
        new Rule(Rule.nextKey, left, right, 0)
    }

    def createAssoc(left: List[GSym], right: List[GSym], cost: Double): Rule = {
        new Rule(Rule.nextKey, left, right, cost)
    }
}
