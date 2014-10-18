package grammar.derivation

import java.util.Comparator
import java.util.function.{ToDoubleFunction, ToIntFunction, ToLongFunction, Function}

import grammar.{Rule, GSym, Grammar}
import scala.collection.mutable
import NLP._
import scala.collection.JavaConverters._
import NLP.SuffixAmt.EditOperation
import utils.Keyable

/**
 * @author A.Sirenko
 *          Date: 9/17/13
 */
class Derivation(val suffixAmt: SuffixAmt, val countOfResultsFromOneSentence: Int) {

  case class OrderedAppliedTrans(t: AppliedTrans)

  implicit def orderedFoo(t: OrderedAppliedTrans): Ordered[OrderedAppliedTrans] = new Ordered[OrderedAppliedTrans] {
    def compare(other: OrderedAppliedTrans) = t.t.reachedCost.compareTo(other.t.reachedCost)
  }

  var posTransCached: Map[CPoint, Set[PosTrans]] = Map.empty

  def compute(query: Query): DerivationResult = {
    posTransCached = Map.empty
    var derivedSymbols = List[Pair[GSym, AppliedTrans]]()
    var points: Set[CPoint] = Set.empty

    val forNextStep: mutable.PriorityQueue[OrderedAppliedTrans] = mutable.PriorityQueue[OrderedAppliedTrans]()

    val ignoreSet: Set[GSym] = query.query.foldLeft(Set[GSym]())((set, symbol) => set + symbol)
    val root: CPoint = new CPoint(query.query)
    points = points + root

    val initialHistory: List[History] = History.initialHistory(query.query.length)

    for (pTrans : PosTrans <- getPossibleTrans(root, query.grammar)) {
        val aTrans = new AppliedTrans(pTrans.rule.cost, Option.empty, 1, pTrans, root, History.buildHistory(initialHistory, pTrans))

        points = points + pTrans.child
        val reachedByTransform: Set[GSym] = pTrans.child.sentence
                .foldLeft(Set[GSym]())((s, v) => if (ignoreSet.contains(v)) s else s + v)

        derivedSymbols = reachedByTransform.foldLeft(derivedSymbols)((l, v) => Pair(v, aTrans) :: l)

        if (aTrans.level <= query.maxLevelOfTransform) {
            forNextStep.enqueue(OrderedAppliedTrans(aTrans))
        }
    }

    while (points.size < query.maxCountOfGeneratedSentences && !forNextStep.isEmpty) {
        val cheapestDerivation = forNextStep.dequeue().t
        val current = cheapestDerivation.posTrans.child

        for (pTrans : PosTrans <- getPossibleTrans(current, query.grammar)) {
            val aTrans = composeNextAppliedTransform(cheapestDerivation, pTrans)

            points = points + pTrans.child
            val reachedByTransform: Set[GSym] = pTrans.child.sentence.foldLeft(Set[GSym]())(
                    (s, v) => if (ignoreSet.contains(v)) s else s + v
            )

            derivedSymbols = reachedByTransform.foldLeft(derivedSymbols)((l, v) => Pair(v, aTrans) :: l)

            if (aTrans.level <= query.maxLevelOfTransform) {
                forNextStep.enqueue(OrderedAppliedTrans(aTrans))
            }
        }
    }
    println("Compose derivation results for " + derivedSymbols.size + "symbols")
    DerivationResult.build(derivedSymbols, query.maxCountOfDerivedSymbols)
  }

  private def composeNextAppliedTransform(parentTrans: AppliedTrans, posTransToApply: PosTrans): AppliedTrans = {
    new AppliedTrans(
      posTransToApply.rule.cost + parentTrans.reachedCost,
      Option(parentTrans), parentTrans.level + 1, posTransToApply, parentTrans.posTrans.child,
      History.buildHistory(parentTrans.hist, posTransToApply)
    )
  }

  private def getPossibleTrans(point: CPoint, grammar: Grammar): Set[PosTrans] = {
    if (!posTransCached.contains(point)) {
      // synonym trans
      var posSet: Set[PosTrans] = Set.empty
      var offset = 0
      for (s <- point.sentence) {
	val synRules = grammar.synRules.getByLeft(s)
	if (synRules != Option.empty) {
	  for (r <- synRules.get) {
	    val child = point.apply(r, offset, r.left.size)
	    posSet = posSet + new PosTrans(r, offset, child)
	  }
	}
	offset = offset + 1
      }

      // cognitive
      val sentence: List[Integer] = point.sentence.foldRight(List.empty[Integer])((a, b) => a.key :: b)
      suffixAmt.buildTree(sentence.asJava)
      val posRules = grammar.cognRules.allRules.filter((p: Rule) => point.sentence.contains(p.left))
      for (r: Rule <- posRules) {
        val results: java.util.List[AmtResult]
        = suffixAmt.treeSearch(r, r.leftKeys.asJava, countOfResultsFromOneSentence)
        for (amt: AmtResult <- results.asScala) {
          assert(r == amt.getRule)
          for (pos <- amt.getPos) {
            val leftReplace: Int = amt.getOperations.foldLeft(r.left.size) (
              (a,b) =>
              if (b == EditOperation.INS) {
                a - 1
              } else if (b == EditOperation.DEL) {
                a + 1
              } else {
                a
              }
            )
            val appliedRule: Rule = Rule.createWeightedCognitive(r.left, r.right, amt.getCost)
            val child: CPoint = point.apply(appliedRule, pos, leftReplace)
            posSet = posSet + new PosTrans(appliedRule, pos, child)
          }
        }
      }

      // assoc
      for (offset <- 0 to point.sentence.length - 1) {
	val sym = point.sentence(offset)
	val rules = grammar.assocRules.getByLeft(sym)
	if (rules != Option.empty && !rules.get.isEmpty) {
	  for(r <- rules.get.filter((r: Rule) => canApplyStrictly(point.sentence, offset, r.left))) {
	    val child = point.apply(r, offset, r.left.size)
	    posSet = posSet + new PosTrans(r, offset, child)
	  }
	}
      }

      posTransCached = posTransCached + (point -> posSet)
    }
    posTransCached.get(point).get
  }

  def canApplyStrictly(sentence: List[GSym], offset: Int, ruleLeft: List[GSym]): Boolean = {
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

object Derivation {

  def createForStrings: Derivation = {
    new Derivation(
      new SuffixAmtStrings(
        AmtDictionaryStrings.newInstance(),
        SuffixAmt.defaultCosts,
        3
      ),
      10
    )
  }

  def createForDictionary(g: Grammar): Derivation = {

    val amt: AmtDictionary[Keyable[Int]] = new AmtDictionary[Keyable[Int]](g.syms.getSymbolsDictionary) {
      override def getName(p1: Int): String = map.get(p1).toString
    }
    val suffixAmt: SuffixAmt = new SuffixAmt(amt, SuffixAmt.defaultCosts(), 3)

    new Derivation(
      suffixAmt,
      10
    )
  }

}
