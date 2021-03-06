package lemms

import org.jetbrains.annotations._
import scala.collection.{mutable, SortedMap}
import wordforms._
import java.util
import java.net.{URLConnection, URL, JarURLConnection}
import java.util.StringTokenizer

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class LemmatizerImpl(val lemmaToWfs: Map[String, List[String]], val wfToLemmas: Map[String,List[String]])
        extends Lemmatizer{

	@NotNull def getLemmas(@NotNull wordform: String): Option[List[String]] = wfToLemmas.get(wordform)

	@NotNull def getWordforms(@NotNull lemma: String): Option[List[String]] = lemmaToWfs.get(lemma)
		
	// TODO use for splitting additional set from StringUtils
	def tokenizeAndLemmatize(sentence: String, keepUnknownWordforms: Boolean, skipStopWords: Boolean): List[String] = {
		val tokens = LemmatizerImpl.tokenize(sentence)
		var res: List[String] = List()
		tokens.foldRight(List.empty[String])((a, b) => {
		    val s = a.toLowerCase(); if (LemmatizerImpl.stopList.contains(s)) {b} else {s :: b}
		}).foreach((raw:String) => {
		    
		    val rawLow = raw.toLowerCase();
			val lemmas = getLemmas(rawLow)
			if (lemmas.isEmpty) {
				if (keepUnknownWordforms) {
					res = res ::: List(rawLow)
				}
			} else if (lemmas.get.length == 1) {
				res = res ::: List(lemmas.get(0))
			}
		})
		
		res
	}

}

object LemmatizerImpl {
    
    val stopList: Set[String] = Set(
        "of", "a", "the", "or", "in", "to", "and", "the", "that", "an", "is", "for", "by", "with", "as",
        "which", "from", "who", "used", "on", "being", "at", "be", "are", "such", "has", "it", "made")

	def create() = {

		var lemmaToWfs: Map[String, List[String]] = Map.empty
		var wfToLemmas: Map[String, List[String]] = Map.empty

		val lemIter = Loader.loadDefaultSet().iterator()
		while(lemIter.hasNext) {
			val l: Lemma = lemIter.next()
			var wfs: List[String] =  List(l.getName)
			val iter = l.getWordforms.iterator()
			while (iter.hasNext) {
				wfs = (iter.next().getName :: wfs).distinct
			}

			if (lemmaToWfs.contains(l.getName)) {
				wfs = (wfs ::: lemmaToWfs.get(l.getName).get).distinct
			}
			lemmaToWfs = lemmaToWfs.updated(l.getName, wfs.distinct)
			for (wf <- wfs) {
				if (wfToLemmas.contains(wf)) {
					wfToLemmas = wfToLemmas.updated(wf, l.getName :: wfToLemmas.get(wf).get)
				} else {
					wfToLemmas = wfToLemmas.updated(wf, List(l.getName))
				}
			}
		}

		new LemmatizerImpl(lemmaToWfs, wfToLemmas)
	}

	def tokenize(sentence: String): List[String] = {
		val t = new StringTokenizer(sentence, "\t;: .,?!'\"\\")
		var res: List[String] = List()
		while(t.hasMoreTokens) {
			res = res ::: List(t.nextToken())
		}
		res
	}

}
