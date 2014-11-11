package grammar.derivation

import grammar.GSym
import play.api.libs.json._


/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */

case class AggrDerivSym(sym: GSym, invCost: Double, srcUsage: Double, chains: List[String]) {
  def toJson: JsObject = {
    Json.obj(
      "gsym" -> sym.toJson, "invCost" -> invCost, "srcUsage" -> srcUsage, "chains" -> chains
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
      sym, obj.\("invCost").as[JsNumber].value.doubleValue(), obj.\("srcUsage").as[JsNumber].value.doubleValue(), chs
    )
  }
}

case class DerivationResult(val aggrSyms: Map[GSym, AggrDerivSym]) {

  def toJson: JsObject = {
    var list: List[JsObject] = List[JsObject]()
    for (obj <- aggrSyms.values) {
      list = obj.toJson :: list
    }
    Json.obj("syms" -> list)
  }

  override def toString: String = "Count of aggregated syms: " + aggrSyms.size
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

  def build(reached: List[Pair[GSym, AppliedTrans]], query: Query): DerivationResult = {
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
    val topSyms: Set[GSym] = sortedSyms.slice(0, Math.min(query.maxCountOfDerivedSymbols, symsWeight.size)).foldLeft(Set.empty[GSym])((a,b)=> a + b._1)
    for (s: GSym <- topSyms) {
        var invCost: Double = 0;
        var used: Set[Int] = Set()
        var chains: List[String] = List();
        for (t: AppliedTrans <- reached.filter(_._1 == s).map(_._2)) {
            invCost += t.reachedCost
            used = t.posTrans.child.sentence.zip(t.hist).filter(_._1 == s).foldLeft(used)((set, b)=> set ++ b._2.sourceUsed)
            chains = t.shortDescription :: chains
        }
        m += (s -> AggrDerivSym(s, invCost, used.size.toDouble / query.query.size, chains))
    }
    new DerivationResult(m)
  }

}
