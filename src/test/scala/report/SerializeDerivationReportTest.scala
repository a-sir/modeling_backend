package report

import org.scalatest.FunSpec
import grammar.report.DerivedSymbol
import grammar.GSym
import java.io.{ByteArrayInputStream, ObjectInputStream, ByteArrayOutputStream, ObjectOutputStream}

/**
 * @author A.Sirenko
 *          Date: 10/4/13
 */
class SerializeDerivationReportTest extends FunSpec {

	describe("A report.Report") {

		it("should be consistent through serialization)") {
			val sym = new grammar.GSym(1, "querysym")
			val query: List[GSym] =  List(sym)
			val report = new grammar.report.DerivationReport(query, 2, 100, List(new DerivedSymbol(1.5, 2, new grammar.GSym(1, "somename"))))
			val byteArray = new ByteArrayOutputStream(1024)
			val oos = new ObjectOutputStream(byteArray)
			oos.writeUnshared(report)
			oos.close()

			val ois = new ObjectInputStream(new ByteArrayInputStream(byteArray.toByteArray))
			val deserialized = ois.readObject()
			assert(report == deserialized)
			ois.close()
		}

	}

}