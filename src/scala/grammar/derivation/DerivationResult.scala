package grammar.derivation

import grammar.Symbol

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
class DerivationResult(val reached: List[Pair[Symbol, AppliedTrans]]) {
	lazy val symbols: Set[Symbol] = reached.foldLeft(Set[Symbol]())((s, v) => s + v._1)
}