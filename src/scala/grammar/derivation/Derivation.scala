package grammar.derivation

import scala.actors.Actor
import grammar.{Symbol, ResultConsumer, Grammar}
import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/17/13
 */
class Derivation(val grammar: Grammar, val query: Query, val resultConsumer: ResultConsumer) extends Actor {

	val cheapTransformComeFirst = new scala.Ordering[AppliedTrans] {
		def compare(x: AppliedTrans, y: AppliedTrans): Int = x.reachedCost.compareTo(y.reachedCost)
	}

	var reached: Set[Symbol] = Set()
	var points: Set[CPoint] = Set()
	var posTrans: Map[CPoint, Set[PosTrans]] = Map()
	var forNextStep: mutable.PriorityQueue[AppliedTrans] = mutable.PriorityQueue.empty(cheapTransformComeFirst)

	def act() {
		val ignoreSet: Set[Symbol] = query.query.foldLeft(Set[Symbol]())((set, symbol) => set + symbol)
		val root: CPoint = new CPoint(query.query)

		for (pTrans : PosTrans <- getPossibleTrans(root)) {
			val aTrans = new AppliedTrans(pTrans.rule.cost, Nil.head, 1, pTrans, root)

			points = points + pTrans.child
			reached = pTrans.child.sentence.foldLeft(reached)((s, v) => if (ignoreSet.contains(v)) s else s + v)

			if (aTrans.level <= query.maxLevelOfTransform) {
				forNextStep.enqueue(aTrans)
			}
		}

		while (points.size < query.maxCountOfGeneratedSentences && !forNextStep.isEmpty) {
			val cheapestDerivation = forNextStep.dequeue()
			val current = cheapestDerivation.posTrans.child

			for (pTrans : PosTrans <- getPossibleTrans(current)) {
				val aTrans = new AppliedTrans(
					pTrans.rule.cost + cheapestDerivation.reachedCost,
					cheapestDerivation, cheapestDerivation.level + 1, pTrans, current
				)

				points = points + pTrans.child
				reached = pTrans.child.sentence.foldLeft(reached)((s, v) => if (ignoreSet.contains(v)) s else s + v)

				if (aTrans.level <= query.maxLevelOfTransform) {
					forNextStep.enqueue(aTrans)
				}
			}
		}

		println("Derivation is done.")
		println("Reached " + reached.size + " symbols")
		println("Derived " + points.size + " points")
	}

	def getPossibleTrans(point: CPoint): Set[PosTrans] = {
		if (posTrans.contains(point)) {
			posTrans.get(point).get
		} else {
			// synonim trans
			var posSet: Set[PosTrans] = Set.empty
			var pos = 0
			for (s <- point.sentence) {
				val synRules = grammar.synRules.getByLeft(s)
				if (synRules != Option.empty) {
					for (r <- synRules.get) {
						val rule: SynRule = new SynRule(r.left, r.right, pos)
						val child = point.apply(rule)
						posSet = posSet + new PosTrans(rule, child)
					}
				}
				pos = pos + 1
			}

			// TODO assocTrans
			// TODO cognTrans
			posTrans = posTrans + (point -> posSet)
			posSet
		}
	}

}
