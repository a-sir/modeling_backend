import org.jetbrains.annotations._
import scala.collection.SortedMap
import wordforms._

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class Lemmatizer(val lemmaToWfs: Map[String, List[String]], val wfToLemmas: Map[String, List[String]]) {

	@NotNull def getLemmas(@NotNull wordform: String): Option[List[String]] = wfToLemmas.get(wordform)

	@Nullable def getSingleLemmaOrEmpty(@NotNull wordform: String): Option[String] = {
		val lemms = wfToLemmas.get(wordform)
		if (lemms == Option.empty || lemms.get.length > 0) {
			Option.empty[String]
		} else {
			Option(lemms.get(0))
		}
	}

	@Nullable def getWordforms(@NotNull lemma: String): Option[List[String]] = lemmaToWfs.get(lemma)

}

object Lemmatizer {

	def create() = {

		val lemmaToWfs: Map[String, List[String]] = SortedMap.empty
		val wfToLemmas: Map[String, List[String]] = SortedMap.empty

		for (l: Lemma <- Loader.loadDefaultSet()) {
			var wfs: List[String] = Nil
			for (wf: WF <- l.getWordforms) {
				wfs = wf.getName :: wfs
			}
			if (lemmaToWfs.contains(l.getName)) {
				wfs = wfs ::: lemmaToWfs.get(l.getName).get
			}
			lemmaToWfs.updated(l.getName, wfs)
			for (wf <- wfs) {
				if (wfToLemmas.contains(wf)) {
					wfToLemmas.updated(wf, l.getName :: wfToLemmas.get(wf).get)
				} else {
					wfToLemmas.updated(wf, l.getName)
				}
			}
		}

		new Lemmatizer(lemmaToWfs, wfToLemmas)
	}
}
