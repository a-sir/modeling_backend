package util

import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
class StringUtilsTest extends FunSpec {

	describe("A StringUtils") {

		it("should detect normal English text (not lexical or semantic level - only set of chars)") {
			assert(StringUtils.isStandartEngText("Hello, my dear 'user@'~"))
			assert(!StringUtils.isStandartEngText("Hello, my dear 'пользователь@'~"))
		}

	}

}
