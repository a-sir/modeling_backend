package util

/**
 * @author A.Sirenko
 *          Date: 6/13/14
 */

import akka.actor._

object AppRunner extends App {
  def confStr(port: Int) = "akka {\n" +
          "//loglevel = \"DEBUG\"\n" +
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
  val conf = com.typesafe.config.ConfigFactory.parseString(confStr(2552))
  val system = ActorSystem("ModelingActorSystem", conf)
  val remoteActor = system.actorOf(Props[InterfaceActor], name = "InterfaceActor")
  remoteActor ! "Started"
}

class InterfaceActor extends Actor {
  def receive = {
    case "Ping" => sender ! "Pong"
    case msg: String => println("InterfaceActor received message " + msg )
  }
}


