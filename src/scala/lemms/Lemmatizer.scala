package lemms

import org.jetbrains.annotations._
import scala.collection.{mutable, SortedMap}
import wordforms._
import java.util
import java.net.{URLConnection, URL, JarURLConnection}

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Lemmatizer(val lemmaToWfs: Map[String, List[String]], val wfToLemmas: Map[String, List[String]]) {

	@NotNull def getLemmas(@NotNull wordform: String): Option[List[String]] = wfToLemmas.get(wordform)

	@Nullable def getWordforms(@NotNull lemma: String): Option[List[String]] = lemmaToWfs.get(lemma)

}

object Lemmatizer {

	def create() = {

		var lemmaToWfs: Map[String, List[String]] = Map.empty
		var wfToLemmas: Map[String, List[String]] = Map.empty

		val lemIter = Loader.loadDefaultSet().iterator()
		while(lemIter.hasNext) {
			val l: Lemma = lemIter.next()
			var wfs: List[String] =  List(l.getName)
			val iter = l.getWordforms.iterator()
			while (iter.hasNext) {
				wfs = iter.next().getName :: wfs
			}

			if (lemmaToWfs.contains(l.getName)) {
				wfs = (wfs ::: lemmaToWfs.get(l.getName).get).distinct
			}
			lemmaToWfs = lemmaToWfs.updated(l.getName, wfs.distinct)
			for (wf <- wfs) {
				if (wfToLemmas.contains(wf)) {
					wfToLemmas = wfToLemmas.updated(wf, (l.getName :: wfToLemmas.get(wf).get).distinct)
				} else {
					wfToLemmas = wfToLemmas.updated(wf, List(l.getName))
				}
			}
		}

		new Lemmatizer(lemmaToWfs, wfToLemmas)
	}
}
