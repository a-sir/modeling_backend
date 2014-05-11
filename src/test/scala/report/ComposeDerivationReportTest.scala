package report

import org.scalatest.FunSpec
import util.NumUtils._
import grammar.Grammar
import grammar.derivation.{Query, Derivation}
/**
 * @author A.Sirenko
 *          Date: 10/6/13
 */
class ComposeDerivationReportTest  extends FunSpec {

	describe("A Report") {

		it("should be aggregate cost like throughput of transport system)") {
			val costs = List[Double](5, 10)
			assert(closeValues(10.0/3.0, grammar.report.DerivationReport.aggregateCost(costs), 0.00001))
		}

		it("should aggregate NaxInt cost for 0 values)") {
			assert(closeValues(Double.MaxValue, grammar.report.DerivationReport.aggregateCost(List.empty), 0.00001))
		}
		
		it("should be built from derivation") {
			val gr = Grammar.createMock()
			val sentence = gr.syms.getOrCreateSymbols(List("a", "c"))
			val query = new Query(sentence, gr, 5, 3)
			val derivation = Derivation.createForStrings
			val derived = derivation.compute(query)
			
			val report: grammar.report.DerivationReport = grammar.report.DerivationReport.compose(query, derived)
			assert(report.symbols.size == 2)
			assert(report.symbols(0).usedSymsFromQuery == 2)
			assert(report.symbols(1).usedSymsFromQuery == 2)
		}
	}

}
