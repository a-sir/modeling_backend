package lemms

import org.jetbrains.annotations.NotNull

/**
 * @author A.Sirenko
 *          Date: 5/18/14
 */
trait Lemmatizer {

    @NotNull def getLemmas(@NotNull wordform: String): Option[List[String]]

    @NotNull def getWordforms(@NotNull lemma: String): Option[List[String]]

    def tokenizeAndLemmatize(sentence: String, keepUnknownWordforms: Boolean, skipStopWords: Boolean): List[String]

}
