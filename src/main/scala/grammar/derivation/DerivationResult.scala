package grammar.derivation

import grammar.GSym
import play.api.libs.json._


/**
 * @author A.Sirenko
 *          Date: 9/19/13
 */
class DerivationResult(val reached: List[Pair[GSym, AppliedTrans]]) {
  lazy val symbols: Set[GSym] = reached.foldLeft(Set[GSym]())((s, v) => s + v._1)
  lazy val aggrSyms: Map[GSym, Pair[Double, List[String]]] = {
    var m: Map[GSym, Pair[Double, List[String]]] = Map.empty
    for(v <- reached) {
      val base: Pair[Double, List[String]] = m.get(v._1) match {
        case x: Some[Pair[Double, List[String]]] => x.get
        case None => Pair(0, List.empty)
      }
      m += (v._1 -> Pair(base._1 + v._2.reachedCost, v._2.chainDescription :: base._2))
    }
    m
  }

  def asTableString(): String = {
    println("Count of aggregated syms: " + aggrSyms.size)
    var i: Int = 0;
    val sb = new StringBuilder()
    for (e <- aggrSyms.seq) {
      println(i)
      i+=1
      sb.append("\n").append(e._1.getKey()).append("\t")
        .append(e._1.name).append("\t")
        .append(e._2._1)
      sb
    }
    sb.toString
  }

  override def toString: String = asTableString()
}
