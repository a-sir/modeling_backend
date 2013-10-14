package grammar

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class CognitiveRule(val left: List[GSym], val right: List[GSym], val context: List[String]) {

	override def toString = "CognRule: " + left + " -> " + right + "[" + context + "]"

}
