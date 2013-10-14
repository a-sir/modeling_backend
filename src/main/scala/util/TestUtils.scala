package util

/**
 * @author A.Sirenko
 *          Date: 9/22/13
 */
object TestUtils {

	def  areSame[T](s1: Set[T], s2: Set[T]): Boolean = {
		if (s1.size != s2.size) {
			return false
		} else {
			for (value <- s1) {
				if (!s2.contains(value)) {
					return false
				}
			}
		}
		true
	}

}
