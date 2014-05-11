package grammar.derivation

import grammar.{GSym, Rule}

/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

case class CPoint(sentence: List[GSym]) {

	def apply(rule: Rule, offset: Int, replaceSize: Int) = new CPoint(sentence.patch(offset, rule.right, replaceSize))

}

case class PosTrans(rule: Rule, offset: Int, child: CPoint)

case class History(posInQuery: Option[Int], sourceUsed: Set[Int]) {
}

object History {

	def buildHistory(old: List[History], trans: PosTrans): List[History] = {
		val usedByCurrentTransform = old.slice(trans.offset, trans.offset + trans.rule.left.length)
				.foldLeft(Set[Int]())(
						(s, h) => {
							if (h.posInQuery == Option.empty)
								s ++ h.sourceUsed
							else 
								s + h.posInQuery.get
						}
				)
		val entry = new History(Option.empty, usedByCurrentTransform)
		val changedPart: List[History] = List.fill(trans.rule.right.length)(entry)
		old.slice(0, trans.offset) ::: changedPart ::: old.slice(trans.offset + trans.rule.left.length, old.length)
	}

	def initialHistory(length: Int) = (0 to length - 1)
			.foldLeft(List[History]())((l, v) => new History(Option(length - v - 1), Set.empty) :: l)

}

case class AppliedTrans(
		reachedCost: Double, parent: Option[AppliedTrans],
		level: Int, posTrans: PosTrans, root: CPoint, hist: List[History]
)