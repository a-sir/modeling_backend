package grammar.derivation

import org.scalatest.FunSpec
import assoc_net.AssociativeNet
import lemms.LemmatizerImpl
import import_ling.CognemReader
import cognems.Cognem
import java.util.concurrent._
import grammar.Grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class ProcessorTest extends FunSpec {
describe("A Processor") {

  it("should derive from apple in English") {
    val g = Grammar.createEnglishGrammar()
    println("Grammar loaded")
    val deriv = Derivation.createForDictionary(g)
    println("Derivator is build")
    val pool: ExecutorService = Executors.newSingleThreadExecutor()
    val processor = new Processor(grammar = g, derivator = deriv)
    pool.execute(processor)
    println("Processor started")
    val sessionId = "identifier123"
    val query = "weight of apple"
    processor.requests.offer("submit:" + sessionId + ":" + query)
    val res: Tuple3[String, String, String] = processor.processed.poll(15, TimeUnit.SECONDS)
    assert(res != null && sessionId.equals(res._1) && query.equals(res._2))
    assert(res._3.length() > 0)
  }

}

}
