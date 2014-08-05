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
      "gsym" -> sym.toJson, "invCost" -> invCost, "chains" -> chains
    )
  }

}

object AggrDerivSym {

  def fromJson(obj: JsObject): AggrDerivSym = {
    val sym: GSym = GSym.fromJson(obj.\("gsym").as[JsObject])
    val chains: Seq[JsValue] = obj.\("chains").as[JsArray].value
    var chs:List[String] = List[String]()
    chains.seq.foreach(a => chs = a.asInstanceOf[JsString].value :: chs)
    AggrDerivSym(
      sym, obj.\("invCost").as[JsNumber].value.doubleValue(), chs
    )
  }
}

case class DerivationResult(val aggrSyms: Map[GSym, AggrDerivSym]) {

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
  def toJson: JsObject = {
    var list: List[JsObject] = List[JsObject]()
    for (obj <- aggrSyms.values) {
      list = obj.toJson :: list
    }
    Json.obj("syms" -> list)
  }

  override def toString: String = asTableString
}

object DerivationResult {

  def fromJson(obj: JsObject): DerivationResult = {
    val syms: Seq[JsValue] = obj.\("syms").as[JsArray].value
    var chs: List[AggrDerivSym] = List[AggrDerivSym]()
    syms.seq.foreach(a => chs = AggrDerivSym.fromJson(a.asInstanceOf[JsObject]) :: chs)
    var m: Map[GSym, AggrDerivSym] = Map.empty
    for (v: AggrDerivSym <- chs) {
      m = m + (v.sym -> v)
    }
    DerivationResult(m)
  }

  def build(reached: List[Pair[GSym, AppliedTrans]]): DerivationResult = {
      var m: Map[GSym, AggrDerivSym] = Map.empty
      for((sym: GSym, trans: AppliedTrans) <- reached) {
        val base: AggrDerivSym = m.get(sym) match {
          case x: Some[AggrDerivSym] => x.get
          case None => AggrDerivSym(sym, 0, List[String]())
        }
        m += (sym -> AggrDerivSym(base.sym, trans.reachedCost, trans.chainDescription :: base.chains))
      }
      new DerivationResult(m)
  }

}
