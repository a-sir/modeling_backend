package grammar.derivation

import grammar.GSym
import play.api.libs.json._
import org.scalatest.FunSpec

/**
 * @author A.Sirenko
 *          Date: 7/27/14
 */
class DerivationResultTest extends FunSpec {
    
  describe("An DerivationResult") {
      
      it("should be parsed from string") {
          val s: String = "{ \"syms\" : [    { \"gsym\" : { \"key\" : 953, \"name\" : \"sell\" }, \"invCost\" : 4.493867557720584, \"srcUsage\" : 0.45, \"chains\" : [ \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.0]->(grease one's palms)=>[grease one's palms]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.012738853503184714]->(cash)=>[cash]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.15286624203821655]->(purchase)=>[purchase]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.03821656050955414]->(pay)=>[pay]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.01910828025477707]->(s ore)=>[s ore]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.4012738853503185]->(sell)=>[sell]\" ] }, { \"gsym\" : { \"key\" : 203, \"name\" : \"pay\" }, \"invCost\" : 2.783597480093978, \"srcUsage\" : 0.45, \"chains\" : [ \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.0]->(grease one's palms)=>[grease one's palms]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.012738853503184714]->(cash)=>[cash]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.15286624203821655]->(purchase)=>[purchase]\", \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.03821656050955414]->(pay)=>[pay]\" ] }, { \"gsym\" : { \"key\" : 373736, \"name\" : \"grease one's palms\" }, \"invCost\" : 0.6449439560107556, \"srcUsage\" : 0.45, \"chains\" : [ \"(h)-[0.4117647058823529]->(money)=>[money];(money)-[0.1271186440677966]->(spend)=>[spend];(spend)-[0.10606060606060606]->(buy)=>[buy];(buy)-[0.0]->(grease one's palms)=>[grease one's palms]\" ] } ]}"
          val obj = Json.parse(s).as[JsObject];
          val res: DerivationResult = DerivationResult.fromJson(obj);
          assert(res.aggrSyms.size == 3)
      }
  }

  describe("An AggrDerivSym") {

    it("should be serialized and parsed from json") {
      val gsym: GSym = new GSym(1, "name1")
      val derivSym = AggrDerivSym(gsym, 0.5, 0.4, List[String]("a->b", "a->c"))
      val jsObject = derivSym.toJson
      val s2: AggrDerivSym = AggrDerivSym.fromJson(jsObject)
      assert(s2.sym == gsym)
    }

  }

  describe("An DerivResult") {

    it("should be serialized and parsed from json") {

      val gsym1: GSym = new GSym(1, "name1")
      val derivSym1 = AggrDerivSym(gsym1, 0.5, 0.4, List[String]("a->b", "a->c"))

      val gsym2: GSym = new GSym(2, "name2")
      val derivSym2 = AggrDerivSym(gsym2, 0.5, 0.4, List[String]("a->b", "a->c"))

      val m: Map[GSym, AggrDerivSym] = Map(gsym1 -> derivSym1, gsym2 -> derivSym2)

      val res: DerivationResult = new DerivationResult(m)
      val jsObject = res.toJson
      val s2: DerivationResult = DerivationResult.fromJson(jsObject)
      assert(s2.aggrSyms.size == res.aggrSyms.size)
    }

  }
}
