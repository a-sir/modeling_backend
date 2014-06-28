package grammar.derivation

import java.util.concurrent._
import java.util._
import scala.concurrent.{Future}
import akka.actor._
import grammar.Grammar


/**
  Receives: sessionId, and settings;
  */

case class Result(val sessionId: String, val query: String, val result: String)

class Processor(
  val grammar: Grammar, val derivator: Derivation,
  val requests: BlockingQueue[String] = new LinkedBlockingQueue(100),
  val listener: (Result) => Unit)
extends Runnable {

  override def run() = {
    println("Processor started.")
    while (true) {
      println("Wait for data from requests queue")
      val s: String = requests.take()
      println("Take from requests queue: " + s)
      if (s.startsWith("submit:")) {
        val sessionQuery = s.substring(7)
        val del = sessionQuery.indexOf(":")
        val sessionId = sessionQuery.substring(0, del);
        val query = sessionQuery.substring(del + 1)

        val syms = grammar.getSymbols(query.split(" ").toList)
        val derivedTerms = derivator.compute(new Query(syms, grammar, 1000, 5)).symbols.mkString(" ")
        val res = Result(sessionId, query, derivedTerms)
        println("Submit to listener: " + res)
        listener(res)
      } else {
        println("Unknown message: " +s)
      }
    }
  }

}
