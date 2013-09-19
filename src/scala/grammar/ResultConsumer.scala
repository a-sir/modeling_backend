package grammar

import grammar.derivation.{DerivationResult, Query}

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
class ResultConsumer {

	def receive(query: Query, result: DerivationResult) {
		println(query + " processed with result: " + result)
	}

}
