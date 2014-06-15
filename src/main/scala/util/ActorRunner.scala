package util

/**
 * @author A.Sirenko
 *          Date: 6/13/14
 */

import akka.actor._

object HelloRemote extends App  {
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
  val remoteActor = system.actorOf(Props[RemoteActor], name = "RemoteActor")
  remoteActor ! "The RemoteActor is alive"
}

class RemoteActor extends Actor {
  def receive = {
    case msg: String =>
        println("RemoteActor received message " + msg)
        sender ! "Hello from the RemoteActor"
  }
}


