package util

/**
 * @author A.Sirenko
 *         Date: 6/13/14
 */

import akka.actor._
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

    val conf = com.typesafe.config.ConfigFactory.parseString(confStr(2552))
    val system = ActorSystem("ModelingActorSystem", conf)
    val remoteActor = system.actorOf(Props[InterfaceActor], name = "InterfaceActor")
    remoteActor ! "Started"
}

class InterfaceActor extends Actor {

    lazy val lemm = LemmatizerImpl.create()
    lazy val dontKeepUnknownWordforms = false

    def receive = {
        case msg: String =>
            println("InterfaceActor received message " + msg)
            if (msg.startsWith("query:")) {
                val json = Json.parse(msg.substring(6))
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
                        "state" -> "failed lemmatization of query"
                    )
                    println("Lemmatization failed")
                }
                AppRunner.tasks.get(sessionId) match {
                    case some: Some[JsObject] =>
                        val o: JsObject = some.get.asInstanceOf[JsObject]
                        sender ! o.toString()
                    case _ => println("No info for session " + sessionId)
                }
            }
    }
}


