package report

import org.scalatest.FunSpec
import grammar.report.Report
import util.NumUtils._
import grammar.Grammar
import grammar.derivation.{Query, Derivation}

/**
 * @author A.Sirenko
 *          Date: 10/6/13
 */
class ComposeReportTest  extends FunSpec {

	describe("A Report") {

		it("should be aggregate cost like throughput of transport system)") {
			val costs = List[Double](5, 10)
			assert(closeValues(10.0/3.0, Report.aggregateCost(costs), 0.00001))
		}

		it("should aggregate NaxInt cost for 0 values)") {
			assert(closeValues(Double.MaxValue, Report.aggregateCost(List.empty), 0.00001))
		}
		
		it("should be built from derivation") {
			val grammar = Grammar.createMock()
			val sentence = grammar.syms.getOrCreateSymbols(List("a", "c"))
			val query = new Query(sentence, grammar, 5, 3)
			val derivation = new Derivation
			val derived = derivation.compute(query)
			
			val report = Report.compose(query, derived)
			Console.println(report)
			assert(report.symbols.length == 2)
		}
	}

}
