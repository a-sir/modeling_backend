package grammar.report

import grammar.GSym
import grammar.derivation.{AppliedTrans, Query, DerivationResult}

/**
 * @author A.Sirenko
 *          Date: 9/29/13
 */
case class Report (
		query: List[GSym], levelLimit: Int, transformLimit: Int, symbols: List[DerivedSymbol])

@serializable
class DerivedSymbol(val aggregatedCost: Double, val usedSymsFromQuery: Int, sym: grammar.GSym)
		extends grammar.GSym(sym.key, sym.name) {

	override def toString = super.toString + " cost:" + aggregatedCost + " usedSyms:" + usedSymsFromQuery

}

object Report {

	def compose(query: Query, deriv: DerivationResult): Report = {
		var syms: List[DerivedSymbol] = List.empty

		for (reached <- deriv.symbols) {
			val transforms: List[AppliedTrans] = deriv.reached.filter(_._1 == reached)
					.foldLeft(List[AppliedTrans]())((list, pair) => pair._2 :: list)
			val aggregatedCost = aggregateCost(transforms.foldLeft(List[Double]())((l, v) => v.reachedCost :: l))
			val sourceUsed = aggregateSourceUsage(
				transforms.foldLeft(List[List[Boolean]]())((ll, v) => v.sourceUsed :: ll),
				query.query.length
			)

			syms = new DerivedSymbol(aggregatedCost, sourceUsed, reached) :: syms
		}

		new Report(query.query, query.maxLevelOfTransform, query.maxCountOfGeneratedSentences, syms)
	}

	def aggregateCost(values: List[Double]): Double = {
		if (values.isEmpty) {
			Double.MaxValue
		} else {
	        1 / values.foldLeft(0.0)((aggr, c) => aggr + 1/c)
		}
	}

	def aggregateSourceUsage(sourceUsed: List[List[Boolean]], totalSymsInQuery: Int): Int =
		sourceUsed.foldLeft(0)((sum, v) => sum + totalSymsInQuery - v.count(_ == false)) / sourceUsed.length

}

