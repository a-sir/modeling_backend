package grammar.derivation

import grammar._

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
case class Query(
		val query: List[GSym],
		val grammar: Grammar,
		val maxCountOfGeneratedSentences: Int,
		val maxLevelOfTransform: Int) {
}
