package grammar.derivation

import java.util.concurrent.{BlockingQueue, Executors, ExecutorService, Callable}
import scala.concurrent.{Future}
import akka.actor._


/**
  Receives: sessionId, and settings;

  */

class Processor (
  val pool: ExecutorService = Executors.newSingleThreadExecutor(),
  val requests: Set[String] = Set.empty
) extends Actor {

  override def receive(msg: Object) {
    msg match {
      case s: String =>
        if (s.startsWith("submit:")) {
          val sessionQuery = s.substring(7)
          if (requests.contains(sessionQuery)) {
            println("Request" + sessionQuery + "is submitted already")
          } else {
            val del = data.indexOf(":")
            val sessionId = data.substring(0, del);
            val query =data.substring(del + 1)
            pool.submit(new ProcessTask(sessionId, query, new Array[Actor]{sender; this}))
            requests += data
          }
        }
      case res: Array[String] =>
        println("Query" + res.mkString(":") + " is done")
        requests -= res(0)
      case _ => println("Unknown message")
    }
  }

  def close {
    pool.shutdown()
  }
}

class ProcessTask (val sessionId: String, val query: String, val receivers: Array[Actor]) extends Callable[Array[String]] {
  def call(): Array[String] = {
    val res = Array[String] {sessionId; "Failed"}
    receiver ! res
    res
  }
}
