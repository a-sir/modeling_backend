package grammar.derivation

import grammar.{Symbol, Rule}

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

class CPoint(val sentence: List[Symbol]) {
	lazy val possibleTrans: Set[PosTrans] = {
		//
		Set[PosTrans]()
	}

}

class PosTrans(val rule: Rule, val ops: String, val range: Pair[Int, Int], val child: CPoint)

class AppliedTrans(
		val reachedCost: Double, val parent: AppliedTrans,
		val level: Int, val posTrans: PosTrans, val root: CPoint
)
