package grammar.derivation

import java.util.concurrent._
import java.util._
import scala.concurrent.{Future}
import akka.actor._
import grammar.Grammar


/**
  Receives: sessionId, and settings;
  */

class Processor(
  val grammar: Grammar, val derivator: Derivation,
  val requests: BlockingQueue[String] = new LinkedBlockingQueue[String](100),
  val processed: BlockingQueue[Tuple3[String, String, String]] = new LinkedBlockingQueue[Tuple3[String, String, String]](100)
  )
extends Runnable {

  override def run() = {
    while (true) {
      val s: String = requests.take()
      if (s.startsWith("submit:")) {
        val sessionQuery = s.substring(7)
        val del = sessionQuery.indexOf(":")
        val sessionId = sessionQuery.substring(0, del);
        val query = sessionQuery.substring(del + 1)

        val syms = grammar.getSymbols(query.split(" ").toList)
        val derivedTerms = derivator.compute(new Query(syms, grammar, 1000, 5)).symbols.mkString(" ")
        processed.add(Tuple3(sessionId, query, derivedTerms))
      } else {
        println("Unknown message: " +s)
      }
    }
  }

}
