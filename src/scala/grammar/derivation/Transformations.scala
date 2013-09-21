package grammar.derivation

import grammar.{Symbol, Rule}

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

case class CPoint(sentence: List[Symbol]) {

	def apply(rule: AssocRule) = new CPoint(sentence.patch(rule.position, rule.right, rule.left.length))

	def apply(rule: CognRule) = new CPoint(sentence.patch(rule.range._1, rule.right, rule.range._2))

	def apply(rule: SynRule) = new CPoint(sentence.patch(rule.position, rule.right, rule.left.length))
}

case class AssocRule(leftSyms: List[Symbol], rightSyms: List[Symbol], costOfRule: Double, position: Int)
		extends Rule(leftSyms, rightSyms,costOfRule)

case class CognRule(
		leftSyms: List[Symbol], rightSyms: List[Symbol], costOfRule: Double, range: Pair[Int, Int], ops: String)
		extends Rule(leftSyms, rightSyms,costOfRule)

case class SynRule(leftSyms: List[Symbol], rightSyms: List[Symbol], position: Int)
		extends Rule(leftSyms, rightSyms, 0)

class PosTrans(val rule: Rule, val child: CPoint)

class AppliedTrans(
		val reachedCost: Double, val parent: AppliedTrans,
		val level: Int, val posTrans: PosTrans, val root: CPoint
)