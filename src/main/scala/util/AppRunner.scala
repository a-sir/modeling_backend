package util

/**
 * @author A.Sirenko
 *         Date: 6/13/14
 */

import java.util.concurrent._
import java.util.Queue
import akka.actor._
import grammar.Grammar
import grammar.derivation._
import lemms.LemmatizerImpl
import play.api.libs.json._

object AppRunner extends App {

    def confStr(port: Int) = "akka {\n" +
            "loglevel = \"DEBUG\"\n" +
            "  actor {\n  provider = \"akka.remote.RemoteActorRefProvider\"\n }\n\n" +
            "  remote {\n" +
            "    enabled-transports = [\"akka.remote.netty.tcp\"]\n" +
            "    netty.tcp {\n" +
            "      hostname = \"127.0.0.1\"\n" +
            "      port = " + port + "\n" +
            "    }\n" +
            "  }" +
            "\n" +
            "}"

    var tasks: Map[String, JsObject] = Map.empty

    def getTaskStatus(sessionId: String): JsObject = {
      Json.obj(
        "sessionId" -> sessionId,
        "status" -> (
          tasks.get(sessionId) match {
              case some: Some[JsObject] => some.get.asInstanceOf[JsObject]
              case _ => JsNull
          }
        )
      )
    }

  val conf = com.typesafe.config.ConfigFactory.parseString(confStr(2552))
  val system = ActorSystem("ModelingActorSystem", conf)
  val remoteActor = system.actorOf(Props[InterfaceActor], name = "InterfaceActor")
  remoteActor ! "Started"

  val g = Grammar.createEnglishGrammar()
  println("Grammar loaded")

  val deriv = Derivation.createForDictionary(g)
  println("Derivator is build")

  val pool: ExecutorService = Executors.newSingleThreadExecutor()
  val processor = new Processor(grammar = g, derivator = deriv)
  pool.execute(processor)
  println("Processor started")
}

class InterfaceActor extends Actor {

    lazy val lemm = LemmatizerImpl.create()

    def receive = {
        case msg: String =>
            println("InterfaceActor received message " + msg)
            if (msg.startsWith("query:")) {
              processQuery(msg.substring(6))
            } else if (msg.startsWith("session_status:")) {
              readProcessed
              sendStatusUpdate(msg.substring(15))
            }
    }

  def readProcessed {

    while (AppRunner.processor.processed.isEmpty()) {
      var t3: Tuple3[String, String, String] = AppRunner.processor.processed.poll(5, TimeUnit.MICROSECONDS)
      // TODO update map
    }
  }

  def sendStatusUpdate(sessionId: String) {
    sender ! "status_update:" + AppRunner.getTaskStatus(sessionId).toString()
  }

  def processQuery(q: String) {
    val json = Json.parse(q)
    val query = (json \ "query").as[String]
    val sessionId = (json \ "sessionId").as[String]
    AppRunner.processor.requests.put("submit:" + sessionId + ":" + query)
    AppRunner.tasks += sessionId -> Json.obj("orig_query" -> query, "state" -> "submitted")
    sendStatusUpdate(sessionId)
  }
}
