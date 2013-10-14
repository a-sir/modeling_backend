package util

/**
 * @author A.Sirenko
 *          Date: 10/6/13
 */
object NumUtils {

	def closeValues(v1: Double, v2: Double, threshold: Double) = (v1-v2).abs <= threshold

}
