package util

/**
 * @author A.Sirenko
 *         Date: 6/13/14
 */

import akka.actor._
import grammar.Grammar
import grammar.derivation.Derivation
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

}

class InterfaceActor extends Actor {

    lazy val lemm = LemmatizerImpl.create()
    lazy val dontKeepUnknownWordforms = false

    def receive = {
        case msg: String =>
            println("InterfaceActor received message " + msg)
            if (msg.startsWith("query:")) {
              processQuery(msg.substring(6))
            } else if (msg.startsWith("session_status:")) {
              sendStatusUpdate(msg.substring(15))
            }
    }

    def sendStatusUpdate(sessionId: String) {
      sender ! "status_update:" + AppRunner.getTaskStatus(sessionId).toString()
    }

    def processQuery(q: String) {
        val json = Json.parse(q)
        val query = (json \ "query").as[String]
        val sessionId = (json \ "sessionId").as[String]

        val l: List[String] = lemm.tokenizeAndLemmatize(query, dontKeepUnknownWordforms)

        if (l.size > 0) {
            AppRunner.tasks += sessionId -> Json.obj(
                "orig_query" -> query,
                "lemmatized_query" -> l.mkString(","),
                "state" -> "lemmatized query"
            )
            println("Lemmatized correctly: " + AppRunner.tasks.get(sessionId))
        } else {
            AppRunner.tasks += sessionId -> Json.obj(
                "orig_query" -> query,
                "state" -> "failed",
                "state_description" -> "failed lemmatization of query"
            )
            println("Lemmatization failed")
        }
        sendStatusUpdate(sessionId)
    }
}


