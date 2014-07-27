package grammar.derivation

import grammar.GSym
import play.api.libs.json._


/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

case class AggrDerivSym(sym: GSym, invCost: Double, chains: List[String]) {
  def toJson: JsObject = {
    Json.obj(
      "symKey" -> sym.getKey, "symName" -> sym.name,
      "invCost" -> invCost,
      "chains" -> chains
    )
  }

}

object AggrDerivSym {
  def fromJson(obj: JsObject): AggrDerivSym = {
    val sym: GSym = new GSym(
      obj.\("symKey").as[JsNumber].value.intValue(),
      obj.\("symName").as[JsString].value
    )
    val chains: Seq[JsValue] = obj.\("chains").as[JsArray].value
    var chs:List[String] = List[String]()
    chains.seq.foreach(a => chs = a.asInstanceOf[JsString].value :: chs)
    AggrDerivSym(
      sym, obj.\("invCost").as[JsNumber].value.doubleValue(), chs
    )
  }
}

class DerivationResult(val reached: List[Pair[GSym, AppliedTrans]]) {
  lazy val symbols: Set[GSym] = reached.foldLeft(Set[GSym]())((s, v) => s + v._1)
  lazy val aggrSyms: Map[GSym, AggrDerivSym] = {
    var m: Map[GSym, AggrDerivSym] = Map.empty
//    for(v <- reached) {
//      val base: AggrDerivSym = m.get(v._1) match {
//        case x: Some[AggrDerivSym] => x.get
//        case None => AggrDerivSym(v._1, 0, List.empty)
//      }
//      m += (v._1 -> AggrDerivSym(base._1, v._2.reachedCost, v._2.chainDescription :: base._2))
//    }
    m
  }

  def asTableString: String = {
    println("Count of aggregated syms: " + aggrSyms.size)
    val sb = new StringBuilder()
//    for (e <- aggrSyms.seq) {
//      sb.append("\n").append(e._1.getKey()).append("\t")
//        .append(e._1.name).append("\t").append(e._2._1)
//      sb
//    }
    sb.toString
  }

  def convertReachedSymsToJson(syms: Seq[AggrDerivSym]): JsValue = {
    JsString("a")
    //Json.toJson(
          //syms.map { t =>
          //    Map("symname" -> t.sym, "tweet" -> t.tweet, "date" -> t.date)
          //}
    //  )
  }

  override def toString: String = asTableString
}
