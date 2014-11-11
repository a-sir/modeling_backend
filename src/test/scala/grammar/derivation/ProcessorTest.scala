package grammar.derivation

import org.scalatest.FunSpec
import java.util.concurrent._
import grammar.Grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class ProcessorTest extends FunSpec {
describe("A Processor") {

  ignore("should derive from apple in English") {
    val g = Grammar.createEnglishGrammar()
    println("Grammar loaded")
    val deriv = Derivation.createForDictionary(g)
    println("Derivator is build")
    val pool: ExecutorService = Executors.newSingleThreadExecutor()
    var res: Result = null
    val processor = new Processor(grammar = g, derivator = deriv, listener = ((r: Result) => res = r))
    pool.execute(processor)
    println("Processor started")
    val sessionId = "identifier123"
    val query = "weight of apple"
    processor.requests.offer("submit:" + sessionId + ":" + query)
    while (res == null) {
      Thread.sleep(100)
    }
    assert(res != null && sessionId.equals(res.sessionId) && query.equals(res.query))
    assert(res.result.aggrSyms.size > 0)
    pool.shutdown()
  }

}

}
