package grammar.derivation

import org.scalatest.FunSpec
import grammar.{GSym, Rule}

/**
 * @author A.Sirenko
 *          Date: 10/6/13
 */
class HistoryTest extends FunSpec {

	describe("A History") {

		it("should create correct initialHistory)") {
			assert(History.initialHistory(3) == List(
							new History(Option(0), Set.empty),
							new History(Option(1), Set.empty),
							new History(Option(2), Set.empty)))
		}

		it("should merge used symbols after transformation)") {

			val originalHistory: List[History] = History.initialHistory(5)

			val syms: List[GSym] = (0 to 10).foldRight(List[GSym]())((c, l) => GSym(c, c.toString) :: l)
			val fakePoint = new CPoint(syms)

			val first: List[History] = History.buildHistory(
				originalHistory,
				new PosTrans(
						Rule.createAssoc(syms.slice(0, 2), syms.slice(0, 3), 0.5),
						0,
						fakePoint
				)
			)

			assert(first == List(
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option(2), Set.empty),
				new History(Option(3), Set.empty),
				new History(Option(4), Set.empty))
			)

			val second: List[History] = History.buildHistory(
				first,
				new PosTrans(Rule.createAssoc(syms.slice(0, 2), syms.slice(0, 1), 1), 4, fakePoint)
			)

			assert(second == List(
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option(2), Set.empty),
				new History(Option.empty, Set(3,4)))
			)

			val third: List[History] = History.buildHistory(
				second,
				new PosTrans(Rule.createAssoc(syms.slice(0, 4), syms.slice(0, 1), 1), 1, fakePoint)
			)

			assert(third == List(
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1,2,3,4)))
			)

			val forth: List[History] = History.buildHistory(
				second,
				new PosTrans(Rule.createAssoc(syms.slice(0, 3), syms.slice(0, 1), 1), 3, fakePoint)
			)

			assert(forth == List(
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(0,1)),
				new History(Option.empty, Set(2,3,4)))
			)

		}

	}

}
