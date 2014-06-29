package grammar.derivation

import grammar.GSym
import play.api.libs.json._


/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
class DerivationResult(val reached: List[Pair[GSym, AppliedTrans]]) {
  lazy val symbols: Set[GSym] = reached.foldLeft(Set[GSym]())((s, v) => s + v._1)
  lazy val aggrSyms: Map[GSym, Double] = {
    var m: Map[GSym, Double] = Map.empty
    for(v <- reached) {
      val base = m.get(v._1) match {
        case x: Some[Double] => x.get
        case None => 0
      }
      m += (v._1 -> (v._2.reachedCost + base))
    }
    m
  }

  def asTableString(): String = {
    aggrSyms.foldLeft(new StringBuilder())((a, b) =>
      a.append("\n").append(b._1.getKey()).append("\t")
        .append(b._1.name).append("\t")
        .append(b._2)
    ).toString
  }
}
