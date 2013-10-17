package grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Rule(val left: List[GSym], val right: List[GSym], val cost: Double) {

	override def toString = "left: " + left + ", right: " + right + "cost: " + cost

}
