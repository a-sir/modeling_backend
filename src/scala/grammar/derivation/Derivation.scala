package grammar.derivation

import scala.actors.Actor
import grammar.{Symbol, ResultConsumer, Grammar}
import scala.collection.immutable.{TreeSet, SortedSet}

/**
 * @author A.Sirenko
 *          Date: 9/17/13
 */
class Derivation(val grammar: Grammar, val query: Query, val resultConsumer: ResultConsumer) extends Actor {

	def act() {
		val ignoreSet: Set[Symbol] = query.query.foldLeft(Set[Symbol]())((set, symbol) => set + symbol)

		var reached: Set[Symbol] = Set()
		var points: Set[CPoint] = Set()
		val ordering = new scala.Ordering[AppliedTrans]{
			def compare(x: AppliedTrans, y: AppliedTrans): Int = x.reachedCost.compareTo(y.reachedCost)
		}
		var forNextStep: Set[AppliedTrans] = TreeSet.empty(ordering)
		val root: CPoint = new CPoint(query.query)

		val posTrans = root.possibleTrans
		for (pTrans : PosTrans <- root.possibleTrans) {
			val aTrans = new AppliedTrans(pTrans.rule.cost, Nil.head, 1, pTrans, root) // why to keep root here?
			forNextStep = forNextStep + aTrans
		}

		var opCount = 0
		while (opCount < query.maxCountOfOperations) {

		}

		grammar.
		println("Let's derive from " + query)
	}

}
