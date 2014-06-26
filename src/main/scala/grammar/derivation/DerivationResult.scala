package grammar.derivation

import grammar.GSym

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
class DerivationResult(val reached: List[Pair[GSym, AppliedTrans]]) {
	lazy val symbols: Set[GSym] = reached.foldLeft(Set[GSym]())((s, v) => s + v._1)
}
