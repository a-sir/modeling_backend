package lemms

import org.jetbrains.annotations._

/**
 * @author A.Sirenko
 *          Date: 5/18/14
 */
class LemmatizerSameImpl extends Lemmatizer {
    @NotNull
    override def getLemmas(wordform: String): Option[List[String]] = new Some[List[String]](List(wordform))

    @NotNull
    override def getWordforms(lemma: String): Option[List[String]] = new Some[List[String]](List(lemma))

    override def tokenizeAndLemmatize(
            sentence: String, keepUnknownWordforms: Boolean, skipStopWords: Boolean)
    : List[String] = sentence.split(' ').toList
}
