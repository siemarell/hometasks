package transaction

import com.google.common.primitives.Bytes
import io.circe.HCursor
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class BDTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {


  property("BDTransactions should be serialized and deserialized") {
    forAll(BDTransactionGenerator) { tx =>
      val recovered = tx.serializer.parseBytes(tx.serializer.toBytes(tx)).get
      tx.serializer.toBytes(tx) shouldEqual tx.serializer.toBytes(recovered)
    }
  }

  property("BDTransactions id should be JSON serialized") {
    forAll(BDTransactionGenerator) { tx =>
      val txJson = tx.json
      val cursor: HCursor = txJson.hcursor
      cursor.downField("id").as[String].getOrElse("")
        .grouped(2)
        .map(Integer.parseInt(_, 16).toByte)
        .toArray shouldEqual tx.id
    }
  }
}