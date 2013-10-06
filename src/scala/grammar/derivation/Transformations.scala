package grammar.derivation

import grammar.{GSym, Rule}

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

case class CPoint(sentence: List[GSym]) {

	def apply(rule: AssocTransform) = new CPoint(sentence.patch(rule.position, rule.right, rule.left.length))

	def apply(rule: CognTransform) = new CPoint(sentence.patch(rule.range._1, rule.right, rule.range._2))

	def apply(rule: SynTransform) = new CPoint(sentence.patch(rule.position, rule.right, rule.left.length))
}

case class AssocTransform(leftSyms: List[GSym], rightSyms: List[GSym], costOfRule: Double, position: Int)
		extends Rule(leftSyms, rightSyms,costOfRule)

case class CognTransform(
		leftSyms: List[GSym], rightSyms: List[GSym], costOfRule: Double, range: Pair[Int, Int], ops: String)
		extends Rule(leftSyms, rightSyms,costOfRule)

object CognTransform {
	val DEFAULT_COST = 0.5
}

object SynTransform {
	val DEFAULT_COST = 0.1
}

case class SynTransform(leftSyms: List[GSym], rightSyms: List[GSym], position: Int)
		extends Rule(leftSyms, rightSyms, SynTransform.DEFAULT_COST)

case class PosTrans(rule: Rule, offset: Int, child: CPoint)

case class AppliedTrans(
		reachedCost: Double, parent: Option[AppliedTrans],
		level: Int, posTrans: PosTrans, root: CPoint, sourceUsed: List[Boolean]
)