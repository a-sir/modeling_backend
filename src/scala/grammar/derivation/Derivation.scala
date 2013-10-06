package grammar.derivation

import grammar.{CognitiveRule, Rule, GSym, Grammar}
import scala.collection.mutable

/**
 * @author A.Sirenko
 *          Date: 9/17/13
 */
class Derivation() {

	val cheapTransformComeFirst = new scala.Ordering[AppliedTrans] {
		def compare(x: AppliedTrans, y: AppliedTrans): Int = x.reachedCost.compareTo(y.reachedCost)
	}

	var posTransCached: Map[CPoint, Set[PosTrans]] = Map()

	def compute(query: Query): DerivationResult = {
		posTransCached = Map()
		var reached: Set[GSym] = Set()
		var derivedSymbols: List[Pair[GSym, AppliedTrans]] = List.empty
		var points: Set[CPoint] = Set()

		val forNextStep: mutable.PriorityQueue[AppliedTrans] = mutable.PriorityQueue.empty(cheapTransformComeFirst)

		val ignoreSet: Set[GSym] = query.query.foldLeft(Set[GSym]())((set, symbol) => set + symbol)
		val root: CPoint = new CPoint(query.query)
		points = points + root

		for (pTrans : PosTrans <- getPossibleTrans(root, query.grammar)) {
			val aTrans = new AppliedTrans(
				pTrans.rule.cost, Option.empty, 1, pTrans, root, List.fill(root.sentence.length){false}
			)

			points = points + pTrans.child
			val reachedByTransform: Set[GSym] = pTrans.child.sentence.foldLeft(Set[GSym]())(
				(s, v) => if (ignoreSet.contains(v)) s else s + v
			)

			reached = reachedByTransform.foldLeft(reached)((s, v) => s + v)
			derivedSymbols = reachedByTransform.foldLeft(derivedSymbols)((l, v) => Pair(v, aTrans) :: l)

			if (aTrans.level <= query.maxLevelOfTransform) {
				forNextStep.enqueue(aTrans)
			}
		}

		while (points.size < query.maxCountOfGeneratedSentences && !forNextStep.isEmpty) {
			val cheapestDerivation = forNextStep.dequeue()
			val current = cheapestDerivation.posTrans.child

			for (pTrans : PosTrans <- getPossibleTrans(current, query.grammar)) {

				val aTrans = composeNextAppliedTransform(cheapestDerivation, pTrans)

				points = points + pTrans.child
				val reachedByTransform: Set[GSym] = pTrans.child.sentence.foldLeft(reached)(
					(s, v) => if (ignoreSet.contains(v)) s else s + v
				)

				reached = reachedByTransform.foldLeft(reached)((s, v) => s + v)
				derivedSymbols = reachedByTransform.foldLeft(derivedSymbols)((l, v) => Pair(v, aTrans) :: l)

				if (aTrans.level <= query.maxLevelOfTransform) {
					forNextStep.enqueue(aTrans)
				}
			}
		}
		new DerivationResult(derivedSymbols)
	}

	private def composeNextAppliedTransform(parentTrans: AppliedTrans, posTransToApply: PosTrans): AppliedTrans = {
		val sourceUsed = parentTrans.sourceUsed.patch(
			posTransToApply.offset,
			List.fill(posTransToApply.rule.right.length) {true},
			posTransToApply.rule.left.length
		)
		new AppliedTrans(
			posTransToApply.rule.cost + parentTrans.reachedCost,
			Option(parentTrans), parentTrans.level + 1, posTransToApply, parentTrans.posTrans.child, sourceUsed
		)
	}

	private def getPossibleTrans(point: CPoint, grammar: Grammar): Set[PosTrans] = {
		if (!posTransCached.contains(point)) {
			// synonim trans
			var posSet: Set[PosTrans] = Set.empty
			var offset = 0
			for (s <- point.sentence) {
				val synRules = grammar.synRules.getByLeft(s)
				if (synRules != Option.empty) {
					for (r <- synRules.get) {
						val rule: SynTransform = new SynTransform(r.left, r.right, offset)
						val child = point.apply(rule)
						posSet = posSet + new PosTrans(rule, offset, child)
					}
				}
				offset = offset + 1
			}

			for (offset <- 0 to point.sentence.length - 1) {
		   		val sym = point.sentence(offset)
				val rules = grammar.assocRules.getByLeft(sym)
				if (rules != Option.empty && !rules.get.isEmpty) {
					for(r <- rules.get.filter((r: Rule) => canApplyStrinctly(point.sentence, offset, r.left))) {
						val transform: AssocTransform = new AssocTransform(r.left, r.right, r.cost, offset)
						val child = point.apply(transform)
						posSet = posSet + new PosTrans(transform, offset, child)
					}
				}
			}

			for (offset <- 0 to point.sentence.length - 1) {
				val sym = point.sentence(offset)

				if (grammar.cognRules.rulesBySense.contains(sym)) {
					val rules: mutable.Set[CognitiveRule] = grammar.cognRules.rulesBySense(sym)
					for(r <- rules.filter((r: CognitiveRule) => canApplyStrinctly(point.sentence, offset, r.left))) {
						val transform: CognTransform = new CognTransform(
							r.left, r.right, CognTransform.DEFAULT_COST, Pair(offset, offset + r.left.length),
							"strict replace"
						)
						val child = point.apply(transform)
						posSet = posSet + new PosTrans(transform, offset, child)
					}
				}
			}
			posTransCached = posTransCached + (point -> posSet)
		}
		posTransCached.get(point).get
	}

	def canApplyStrinctly(sentence: List[GSym], offset: Int, ruleLeft: List[GSym]): Boolean = {
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
