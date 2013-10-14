package import_ling

import java.util.zip.GZIPInputStream
import java.io._
import cognems.Cognem
import util.StringUtils._

/**
 * @author A.Sirenko
 *          Date: 9/14/13
 */
object CognemReader {

	val DEFAULT_WIKITIONARY_PARSED = "data/wikitionary.parsed.gz"

	lazy val defaultSet : List[Cognem] = read(DEFAULT_WIKITIONARY_PARSED)

	def parse(lines: List[String]): List[Cognem] = {
		if (!lines.head.startsWith("# ") || lines.length < 2) {
			throw new RuntimeException("Wrong lines: " + lines)
		}
		val name = lines.head.substring(2)
		var res: List[Cognem] = List()
		for (i <- 1 to lines.length - 1) {
			val comp = lines(i).split('|')
			val desc = comp(1)
			var areas: Array[String] = comp(0).split(',')
			if (areas.length == 1 && areas(0) == "---") {
				areas = Array.empty
			}
			res = new Cognem(name, desc, areas) :: res
		}
		res
	}

	def read(path: String): List[cognems.Cognem] = {
		var cognems: List[Cognem] = List()
		var currentCognRaw: List[String] = List()
		val br = new BufferedReader(new InputStreamReader(
			new GZIPInputStream(new FileInputStream(path))
		))
		var line: String = ""
		while (true) {
			line = br.readLine
			if (line == null || line.startsWith("# ")) {
				if (!currentCognRaw.isEmpty) {
					cognems = CognemReader.parse(currentCognRaw.reverse) ::: cognems
				}
				if (line == null) {
					return cognems
				}
				currentCognRaw = List(line)
			} else {
				if (currentCognRaw.isEmpty) {
					throw new RuntimeException("Description of cognema was met before it's name")
				} else {
					currentCognRaw = line :: currentCognRaw
				}
			}
		}
		throw new RuntimeException("Missed EOF")
	}

	def filterCognemsByChars(cognems: List[Cognem]): List[Cognem] = {
		cognems.filter(
			(c) =>
					isStandartEngText(c.name)
					&& isStandartEngText(c.sense)
		)

	}
}