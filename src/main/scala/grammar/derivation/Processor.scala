package grammar.derivation

import java.util.concurrent.{BlockingQueue, Executors, ExecutorService, Callable}
import scala.concurrent.{Future}
import akka.actor._


/**
  Receives: sessionId, and settings;

  */

class Processor (
  val pool: ExecutorService = Executors.newSingleThreadExecutor(),
  var requests: Set[String] = Set.empty
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
          pool.submit(new ProcessTask(sessionId, query, receivers))
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
  val sessionId: String, val query: String, val receivers: List[ActorRef])
    extends Runnable {
  def run() = {
    val res: Array[String] = Array(sessionId, "Failed")
    receivers.foreach {_ ! res}
  }

  def process() {
    // prepare GSym from grammar, prepare query and send to deriv
    // deriv.compute(query:Query)
    println("Done")
  }
}
