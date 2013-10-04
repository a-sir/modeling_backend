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

case class SynTransform(leftSyms: List[GSym], rightSyms: List[GSym], position: Int)
		extends Rule(leftSyms, rightSyms, 0)

class PosTrans(val rule: Rule, val child: CPoint)

class AppliedTrans(
		val reachedCost: Double, val parent: Option[AppliedTrans],
		val level: Int, val posTrans: PosTrans, val root: CPoint
)