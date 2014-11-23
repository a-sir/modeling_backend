package grammar.derivation

import org.scalatest.FunSpec
import grammar.GSym

class TransformationTest extends FunSpec {
  describe("A Transformation") {

    it("CPoint should filter rules") {
        var big: List[GSym] = List.empty;
        big = new GSym(1, "one") :: big;
        big = new GSym(2, "two") :: big;
        big = new GSym(3, "three") :: big;
        big = new GSym(4, "four") :: big;
        val point = new CPoint(big)
    
        var small: List[GSym] = List.empty;
        small = new GSym(3, "one") :: small;
        small = new GSym(2, "five") :: small;
        println(point.containsAnySymbolFrom(small))
    }

  }
}