package grammar.report

import grammar.GSym

/**
 * @author A.Sirenko
 *          Date: 9/29/13
 */
case class Report (
		query: List[GSym], levelLimit: Int, transformLimit: Int, symbols: List[DerivedSymbol])

@serializable
class DerivedSymbol(val aggregatedCost: Double, val usedSymsFromQuery: Int, sym: grammar.GSym)
		extends grammar.GSym(sym.key, sym.name)

