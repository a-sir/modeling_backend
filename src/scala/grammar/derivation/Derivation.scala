package grammar.derivation

import grammar.{CognitiveRule, Rule, Symbol, Grammar}
import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/17/13
 */
class Derivation() {

	val cheapTransformComeFirst = new scala.Ordering[AppliedTrans] {
		def compare(x: AppliedTrans, y: AppliedTrans): Int = x.reachedCost.compareTo(y.reachedCost)
	}

	var posTrans: Map[CPoint, Set[PosTrans]] = Map()

	def compute(query: Query): Set[Symbol] = {
		posTrans = Map()
		var reached: Set[Symbol] = Set()
		var points: Set[CPoint] = Set()

		var forNextStep: mutable.PriorityQueue[AppliedTrans] = mutable.PriorityQueue.empty(cheapTransformComeFirst)

		val ignoreSet: Set[Symbol] = query.query.foldLeft(Set[Symbol]())((set, symbol) => set + symbol)
		val root: CPoint = new CPoint(query.query)
		points = points + root

		for (pTrans : PosTrans <- getPossibleTrans(root, query.grammar)) {
			val aTrans = new AppliedTrans(pTrans.rule.cost, Option.empty, 1, pTrans, root)

			points = points + pTrans.child
			reached = pTrans.child.sentence.foldLeft(reached)((s, v) => if (ignoreSet.contains(v)) s else s + v)

			if (aTrans.level <= query.maxLevelOfTransform) {
				forNextStep.enqueue(aTrans)
			}
		}

		while (points.size < query.maxCountOfGeneratedSentences && !forNextStep.isEmpty) {
			val cheapestDerivation = forNextStep.dequeue()
			val current = cheapestDerivation.posTrans.child

			for (pTrans : PosTrans <- getPossibleTrans(current, query.grammar)) {
				val aTrans = new AppliedTrans(
					pTrans.rule.cost + cheapestDerivation.reachedCost,
					Option(cheapestDerivation), cheapestDerivation.level + 1, pTrans, current
				)

				points = points + pTrans.child
				reached = pTrans.child.sentence.foldLeft(reached)((s, v) => if (ignoreSet.contains(v)) s else s + v)

				if (aTrans.level <= query.maxLevelOfTransform) {
					forNextStep.enqueue(aTrans)
				}
			}
		}
		reached
	}

	private def getPossibleTrans(point: CPoint, grammar: Grammar): Set[PosTrans] = {
		if (!posTrans.contains(point)) {
			// synonim trans
			var posSet: Set[PosTrans] = Set.empty
			var pos = 0
			for (s <- point.sentence) {
				val synRules = grammar.synRules.getByLeft(s)
				if (synRules != Option.empty) {
					for (r <- synRules.get) {
						val rule: SynTransform = new SynTransform(r.left, r.right, pos)
						val child = point.apply(rule)
						posSet = posSet + new PosTrans(rule, child)
					}
				}
				pos = pos + 1
			}

			for (i <- 0 to point.sentence.length - 1) {
		   		val sym = point.sentence(i)
				val rules = grammar.assocRules.getByLeft(sym)
				if (rules != Option.empty && !rules.get.isEmpty) {
					for(r <- rules.get.filter((r: Rule) => canApplyStrinctly(point.sentence, i, r.left))) {
						val transform: AssocTransform = new AssocTransform(r.left, r.right, r.cost, i)
						val child = point.apply(transform)
						posSet = posSet + new PosTrans(transform, child)
					}
				}
			}

			for (i <- 0 to point.sentence.length - 1) {
				val sym = point.sentence(i)

				if (grammar.cognRules.rulesBySense.contains(sym)) {
					val rules: mutable.Set[CognitiveRule] = grammar.cognRules.rulesBySense(sym)
					for(r <- rules.filter((r: CognitiveRule) => canApplyStrinctly(point.sentence, i, r.left))) {
						val transform: CognTransform = new CognTransform(
							r.left, r.right, CognTransform.DEFAULT_COST, Pair(i, i + r.left.length),
							"strict replace"
						)
						val child = point.apply(transform)
						posSet = posSet + new PosTrans(transform, child)
					}
				}
			}
			posTrans = posTrans + (point -> posSet)
		}
		posTrans.get(point).get
	}

	def canApplyStrinctly(sentence: List[Symbol], offset: Int, ruleLeft: List[Symbol]): Boolean = {
		if (ruleLeft.length > sentence.length - offset) {
			return false
		} else {
			for (i <- 0 to ruleLeft.length - 1) {
				if (ruleLeft(i) != sentence(offset + i)) {
					return false
				}
			}
		}
		true
	}
}