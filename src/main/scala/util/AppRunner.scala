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
  var sessionToRemoteActor: Map[String, ActorRef] = Map.empty

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

  def applyProcessed(res: Result) {
    tasks.get(res.sessionId) match {
      case None => println("[" + res.sessionId + ":" + res.query + "] was removed from tasks already. Ignore results")
      case opt: Some[JsObject] =>
        val o = opt.get
        tasks += res.sessionId -> (o + ("derivation_result" -> JsString(res.result)))
        println("Tasks after getting processing results:" + AppRunner.tasks)
    }
  }

  def submitQuery(q: String, sender: ActorRef): String = {
    val json = Json.parse(q)
    val query = (json \ "query").as[String]
    val sessionId = (json \ "sessionId").as[String]
    AppRunner.processor.requests.put("submit:" + sessionId + ":" + query)
    AppRunner.tasks += sessionId -> Json.obj("orig_query" -> query, "state" -> "submitted")
    // TODO if remote actor initiate request, then I don't need this map
    AppRunner.sessionToRemoteActor += sessionId -> sender
    println("Tasks after sumbission:" + AppRunner.tasks)
    sessionId
  }

  val g = Grammar.createEnglishGrammar()
  println("Grammar loaded")

  val deriv = Derivation.createForDictionary(g)
  println("Derivator is build")

  val pool: ExecutorService = Executors.newSingleThreadExecutor()
  val processor = new Processor(grammar = g, derivator = deriv, listener = applyProcessed)
  pool.execute(processor)
  println("Processor started")

  val conf = com.typesafe.config.ConfigFactory.parseString(confStr(2552))
  val system = ActorSystem("ModelingActorSystem", conf)
  val remoteActor = system.actorOf(Props[InterfaceActor], name = "InterfaceActor")
  remoteActor ! "Started"

  // TODO shutdown pool
}

class InterfaceActor extends Actor {

  lazy val lemm = LemmatizerImpl.create()

  def receive = {
    case msg: String =>
      println("InterfaceActor received message " + msg)
      if (msg.startsWith("query:")) {
        val sessionId = AppRunner.submitQuery(msg.substring(6), sender)
        sendStatusUpdate(sessionId)
      } else if (msg.startsWith("session_status:")) {
        sendStatusUpdate(msg.substring(15))
      }
  }

  def sendStatusUpdate(sessionId: String) {
    sender ! "status_update:" + AppRunner.getTaskStatus(sessionId).toString()
  }

}
