package util

/**
 * @author A.Sirenko
 *          Date: 9/15/13
 */
object StringUtils {

	val additionalChars = "\t .,-+!$%&@#*~-+=?/()[]{}:;\"'<>".toCharArray.foldLeft(Set[Char]())((l, c) => l + c)

	def isEnglishLetter(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

	def isStandartEngText(text: String) = text.forall(
			p => p.isWhitespace || p.isDigit || isEnglishLetter(p) || additionalChars.contains(p)
	)

}
