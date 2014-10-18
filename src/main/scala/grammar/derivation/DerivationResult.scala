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

  def build(reached: List[Pair[GSym, AppliedTrans]], limit: Int): DerivationResult = {
    var symsWeight: Map[GSym, Double] = Map.empty
    for((sym: GSym, trans: AppliedTrans) <- reached) {      
      val baseWeight: Double = symsWeight.get(sym) match {
        case x: Some[Double] => x.get
        case None => 0
      }
      symsWeight += (sym -> (trans.reachedCost + baseWeight))
    }

    val sortedSyms: List[(GSym, Double)] = symsWeight.toList.sortBy(-1 * _._2)
    var m: Map[GSym, AggrDerivSym] = Map.empty
    val topSyms: Set[GSym] = sortedSyms.slice(0, Math.min(limit, symsWeight.size)).foldLeft(Set.empty[GSym])((a,b)=> a + b._1)
    for (s: GSym <- topSyms) {
        var invCost: Double = 0;
        var chains: List[String] = List();
        for (t: AppliedTrans <- reached.filter(_._1 == s).map(_._2)) {
            invCost += t.reachedCost
            chains = t.toString :: chains
        }
        m += (s -> AggrDerivSym(s, invCost, chains))
    }
    new DerivationResult(m)
  }

}
