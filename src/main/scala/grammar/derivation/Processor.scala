package grammar.derivation

import java.util.concurrent.{BlockingQueue, Executors, ExecutorService, Callable}
import scala.concurrent.{Future}
import akka.actor._
import grammar.Grammar


/**
  Receives: sessionId, and settings;
  */

class Processor (
  val pool: ExecutorService = Executors.newSingleThreadExecutor(),
  var requests: Set[String] = Set.empty,
  val grammar: Grammar, val derivator: Derivation
) extends Actor {

  override def receive = {
    case s: String =>
      if (s.startsWith("submit:")) {
        val sessionQuery = s.substring(7)
        if (requests.contains(sessionQuery)) {
          println("Request" + sessionQuery + "is submitted already")
        } else {
          val del = sessionQuery.indexOf(":")
          val sessionId = sessionQuery.substring(0, del);
          val query = sessionQuery.substring(del + 1)
          val receivers: List[ActorRef] = List(sender, this.sender())
          pool.submit(new ProcessTask(sessionId, query, receivers, grammar, derivator))
          requests += sessionQuery
        }
      }
    case res: Array[String] =>
      println("Query" + res.mkString(":") + " is done")
      requests -= res(0)
    case _ => println("Unknown message")
  }

  def close {
    pool.shutdown()
  }
}

class ProcessTask (
  val sessionId: String, val query: String, val receivers: List[ActorRef],
  val grammar: Grammar, val deriv: Derivation)
    extends Runnable {
  def run() = {
    val res: Array[String] = Array(sessionId, process)
    receivers.foreach {_ ! res}
  }

  def process(): String = {
    val syms = grammar.getSymbols(query.split(" ").toList)
    val res = deriv.compute(new Query(syms, grammar, 1000, 5)).symbols.mkString(" ")
    println(res)
    res
  }
}
