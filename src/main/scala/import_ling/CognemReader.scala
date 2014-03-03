package import_ling

import cognems.Cognem
import util.StringUtils._
import scala.collection.JavaConverters._

/**
 * @author A.Sirenko
 *          Date: 9/14/13
 */
object CognemReader {

	lazy val defaultSet : List[Cognem] = Cognem.loadDefault().asScala.toList

	def filterCognemsByChars(cognems: List[Cognem]): List[Cognem] = {
		cognems.filter(
			(c) =>
					isStandartEngText(c.name)
					&& isStandartEngText(c.sense)
		)

	}
}